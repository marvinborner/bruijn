module Eval
  ( evalMain
  ) where

import           Binary
import           Control.Exception
import           Control.Monad.State
import qualified Control.Monad.State.Strict    as StrictState
import qualified Data.BitString                as Bit
import qualified Data.ByteString               as Byte
import           Data.List
import qualified Data.Map                      as M
import           Data.Maybe
import           Helper
import           Parser
import           Paths_bruijn
import           Reducer
import           System.Console.Haskeline
import           System.Directory
import           System.Environment
import           System.FilePath.Posix          ( takeBaseName )
import           Text.Megaparsec         hiding ( State
                                                , try
                                                )

data EnvState = EnvState
  { _env   :: Environment
  , _conf  :: EvalConf
  , _cache :: EnvCache
  }
type M = StrictState.StateT EnvState IO

-- why isn't this in Prelude??
split :: (Eq a) => [a] -> [a] -> [[a]]
split _  [] = []
split [] x  = map (: []) x
split a@(_ : _) b@(c : _)
  | Just suffix <- a `stripPrefix` b = [] : split a suffix
  | null rest                        = [[c]]
  | otherwise                        = (c : head rest) : tail rest
  where rest = split a $ tail b

-- TODO: Force naming convention for namespaces/files
loadFile :: String -> EvalConf -> EnvCache -> IO EnvState
loadFile path conf cache = do
  f <- try $ readFile path :: IO (Either IOError String)
  case f of
    Left exception ->
      print
          (ContextualError (ImportError $ show (exception :: IOError))
                           (Context "" $ nicePath conf)
          )
        >> pure (EnvState (Environment []) conf cache)
    Right f' -> eval
      (filter (not . null) $ split "\n\n" f')
      (EnvState
        (Environment [])
        (conf { isRepl = False, evalPaths = (path : (evalPaths conf)) })
        cache
      )

evalIdent :: String -> Environment -> Program (Failable Expression)
evalIdent ident (Environment sub) = state $ \env@(Environment e) ->
  let lookup' name env' = case lookup name env' of
        Nothing -> Left $ UndeclaredIdentifier name
        Just x  -> Right x
  in  case lookup' ident (map fst sub) of -- search in sub env
        s@(Right _) -> (s, env)
        _           -> (lookup' ident (map fst e), env) -- search in global env

evalFun :: Identifier -> Environment -> Program (Failable Expression)
evalFun = evalIdent . functionName

evalAbs :: Expression -> Environment -> Program (Failable Expression)
evalAbs e sub = evalExp e sub >>= pure . fmap Abstraction

evalApp
  :: Expression -> Expression -> Environment -> Program (Failable Expression)
evalApp f g sub =
  evalExp f sub
    >>= (\case
          Left  e  -> pure $ Left e
          Right f' -> fmap (Application f') <$> evalExp g sub
        )

evalInfix
  :: Expression
  -> Identifier
  -> Expression
  -> Environment
  -> Program (Failable Expression)
evalInfix le i re = evalExp $ Application (Application (Function i) le) re

evalPrefix
  :: Identifier -> Expression -> Environment -> Program (Failable Expression)
evalPrefix p e = evalExp $ Application (Function p) e

evalExp :: Expression -> Environment -> Program (Failable Expression)
evalExp idx@(Bruijn      _  ) = const $ pure $ Right idx
evalExp (    Function    fun) = evalFun fun
evalExp (    Abstraction e  ) = evalAbs e
evalExp (    Application f g) = evalApp f g
evalExp (    Infix le i re  ) = evalInfix le i re
evalExp (    Prefix p e     ) = evalPrefix p e

evalDefine
  :: Identifier -> Expression -> Environment -> Program (Failable Expression)
evalDefine i e sub =
  evalExp e sub
    >>= (\case
          Left e' -> pure $ Left e'
          Right f ->
            modify
                (\(Environment s) ->
                  Environment $ ((name, f), Environment []) : s
                )
              >> pure (Right f)
        )
  where name = functionName i

evalTest
  :: Expression -> Expression -> Environment -> Program (Failable Instruction)
evalTest e1 e2 sub =
  evalExp e1 sub
    >>= (\case
          Left  err -> pure $ Left err
          Right e1' -> fmap (Test e1') <$> evalExp e2 sub
        )

evalSubEnv :: [Instruction] -> EnvState -> IO EnvState
evalSubEnv [] s = return s
evalSubEnv (instr : is) s =
  handleInterrupt (putStrLn "<aborted>" >> return s)
    $ evalInstruction instr s (evalSubEnv is)

fullPath :: String -> IO String
fullPath path = do
  lib    <- getDataFileName path -- TODO: Use actual lib directory
  exists <- doesFileExist lib
  pure $ if exists then lib else path

evalInstruction
  :: Instruction -> EnvState -> (EnvState -> IO EnvState) -> IO EnvState
evalInstruction (ContextualInstruction instr inp) s@(EnvState env conf cache) rec
  = case instr of
    Define i e sub -> do
      EnvState subEnv _ _ <- evalSubEnv sub s
      print i
      (res, env') <- pure $ evalDefine i e subEnv `runState` env
      case res of
        Left err ->
          print (ContextualError err $ Context inp $ nicePath conf) >> pure s -- don't continue
        Right _
          | isRepl conf -> (putStrLn $ show i <> " = " <> show e)
          >> return s { _env = env' }
          | otherwise -> rec s { _env = env' }
    Input path -> do
      full <- fullPath path
      if full `elem` evalPaths conf
        then
          print
              (ContextualError (ImportError path) (Context inp $ nicePath conf))
            >> pure s
        else if M.member path (_imported cache)
          then
            let env' = fromJust $ M.lookup path (_imported cache)
            in  rec s { _env = env' <> env }
          else do
            EnvState env' _ cache' <- loadFile full
                                               (conf { nicePath = path })
                                               cache -- TODO: Fix wrong `within` in import error
            cache'' <- pure $ cache
              { _imported = M.insert path env'
                              $ M.union (_imported cache) (_imported cache')
              }
            rec $ EnvState (env' <> env) conf cache'' -- import => isRepl = False
    -- TODO: Don't import subimports into main env
    Import path namespace -> do -- TODO: Merge with Input (very similar)
      full <- fullPath path
      if full `elem` evalPaths conf
        then
          print
              (ContextualError (ImportError path) (Context inp $ nicePath conf))
            >> pure s
        else if M.member path (_imported cache)
          then
            let env' = fromJust $ M.lookup path (_imported cache)
                prefix | null namespace   = takeBaseName path ++ "."
                       | namespace == "." = ""
                       | otherwise        = namespace ++ "."
                env'' = Environment $ map
                  (\((n, e), o) -> ((prefix ++ n, e), o))
                  ((\(Environment e) -> e) env') -- TODO: Improve
            in  rec s { _env = env'' <> env }
          else do
            EnvState env' _ cache' <- loadFile full
                                               (conf { nicePath = path })
                                               cache -- TODO: Fix wrong `within` in import error
            cache'' <- pure $ cache
              { _imported = M.insert path env'
                              $ M.union (_imported cache) (_imported cache')
              }
            let prefix | null namespace   = takeBaseName path ++ "."
                       | namespace == "." = ""
                       | otherwise        = namespace ++ "."
            env'' <- pure $ Environment $ map
              (\((n, e), o) -> ((prefix ++ n, e), o))
              ((\(Environment e) -> e) env') -- TODO: Improve
            rec $ EnvState (env'' <> env) conf cache'' -- import => isRepl = False
    Evaluate e ->
      let (res, _) = evalExp e (Environment []) `runState` env
      in  putStrLn
              (case res of
                Left err -> show err
                Right e' ->
                  "<> "
                    <> (show e')
                    <> "\n*> "
                    <> (show reduced)
                    <> "\n?> "
                    <> (humanifyExpression reduced)
                    <> "\n#> "
                    <> (matchingFunctions reduced env)
                  where reduced = reduce e'
              )
            >> rec s
    Test e1 e2
      | evalTests conf
      -> let (res, _) = evalTest e1 e2 (Environment []) `runState` env
         in
           case res of
             Left err ->
               print (ContextualError err $ Context inp $ nicePath conf)
                 >> pure s
             Right (Test e1' e2') ->
               when (lhs /= rhs) (print $ FailedTest e1 e2 lhs rhs) >> rec s
              where
               lhs = reduce e1'
               rhs = reduce e2'
             _ -> rec s
      | otherwise
      -> rec s
    _ -> rec s
evalInstruction instr s rec =
  evalInstruction (ContextualInstruction instr "<unknown>") s rec

eval :: [String] -> EnvState -> IO EnvState
eval []   s = return s
eval [""] s = return s
eval (block : bs) s@(EnvState _ conf _) =
  handleInterrupt (putStrLn "<aborted>" >> return s)
    $ case parse blockParser "" block of
        Left err ->
          print
              (ContextualError (SyntaxError $ printBundle err)
                               (Context "" $ nicePath conf)
              )
            >> eval bs s
        Right instr -> evalInstruction instr s (eval bs)
 where
  blockParser | isRepl conf = parseReplLine
              | otherwise   = parseBlock 0

evalMainFunc :: Environment -> Expression -> Maybe Expression
evalMainFunc (Environment env) arg = do
  e <- lookup "main" (map fst env)
  pure $ reduce $ Application e arg

evalFileConf
  :: String -> (a -> IO ()) -> (Expression -> a) -> EvalConf -> IO ()
evalFileConf path wr conv conf = do
  EnvState env _ _ <- loadFile path conf (EnvCache M.empty)
  arg              <- encodeStdin
  case evalMainFunc env arg of
    Nothing ->
      print $ ContextualError (UndeclaredIdentifier "main") (Context "" path)
    Just e -> wr $ conv e

defaultConf :: String -> EvalConf
defaultConf path =
  EvalConf { isRepl = False, evalTests = True, nicePath = path, evalPaths = [] }

dumpFile :: String -> (a -> IO ()) -> (Expression -> a) -> IO ()
dumpFile path wr conv = do
  EnvState (Environment env) _ _ <- loadFile path
                                             (defaultConf path)
                                             (EnvCache M.empty)
  case lookup "main" (map fst env) of
    Nothing ->
      print $ ContextualError (UndeclaredIdentifier "main") (Context "" path)
    Just e -> wr $ conv e

evalFile :: String -> (a -> IO ()) -> (Expression -> a) -> IO ()
evalFile path wr conv = evalFileConf path wr conv (defaultConf path)

-- TODO: Merge with evalFile
evalYolo :: String -> (a -> IO ()) -> (Expression -> a) -> IO ()
evalYolo path wr conv =
  evalFileConf path wr conv (defaultConf path) { evalTests = False }

exec :: String -> (String -> IO (Either IOError a)) -> (a -> String) -> IO ()
exec path rd conv = do
  f   <- rd path
  arg <- encodeStdin
  case f of
    Left  exception -> print (exception :: IOError)
    Right f'        -> putStr $ humanifyExpression $ reduce $ Application
      (fromBinary $ conv f')
      arg

repl :: EnvState -> InputT M ()
repl (EnvState env conf cache) =
  (handleInterrupt (return $ Just "") $ withInterrupt $ getInputLine
      "\ESC[36mλ\ESC[0m "
    )
    >>= (\case -- TODO: Add non-parser error support for REPL
          Nothing   -> return ()
          Just line -> do -- setting imported [] for better debugging
            s' <- liftIO
              $ eval [line] (EnvState env conf cache { _imported = M.empty })
            lift $ StrictState.put s'
            repl s'
        )

lookupCompletion :: String -> M [Completion]
lookupCompletion str = do
  (EnvState (Environment env) _ _) <- StrictState.get
  return $ map (\((s, _), _) -> Completion s s False) $ filter
    (\((s, _), _) -> str `isPrefixOf` s)
    env

completionSettings :: String -> Settings M
completionSettings history = Settings
  { complete       = completeWord Nothing " \n" lookupCompletion
  , historyFile    = Just history
  , autoAddHistory = True
  }

runRepl :: IO ()
runRepl = do
  config  <- getDataFileName "config"
  history <- getDataFileName "history"
  prefs   <- readPrefs config
  let -- TODO: Use -y in repl for YOLO lifestyle
      conf = EvalConf { isRepl    = True
                      , evalTests = True
                      , nicePath  = "<repl>"
                      , evalPaths = []
                      }
      looper = runInputTWithPrefs
        prefs
        (completionSettings history)
        (withInterrupt $ repl $ EnvState (Environment [])
                                         conf
                                         (EnvCache M.empty)
        )
  code <- StrictState.evalStateT
    looper
    (EnvState (Environment []) conf (EnvCache M.empty))
  return code

usage :: IO ()
usage = do
  putStrLn "Invalid arguments. Use 'bruijn [option] path' instead"
  putStrLn "-o\toptimize path"
  putStrLn "-c\tcompress path to binary-BLC"
  putStrLn "-C\tcompress path to ASCII-BLC"
  putStrLn "-b\tcompile path to binary-BLC"
  putStrLn "-B\tcompile path to ASCII-BLC"
  putStrLn "-e\texecute path as binary-BLC"
  putStrLn "-E\texecute path as ASCII-BLC"
  putStrLn "-y\tdisable execution of tests - YOLO"
  putStrLn "-*\tshow this help"
  putStrLn "<default>\texecute path as text-bruijn"

evalMain :: IO ()
evalMain = do
  -- TODO: use actual args parser
  args <- getArgs
  case args of
    []           -> runRepl
    ["-b", path] -> dumpFile path
                             (Byte.putStr . Bit.realizeBitStringStrict)
                             (toBitString . toBinary)
    ["-B", path] -> dumpFile path putStrLn toBinary
    ["-e", path] ->
      exec path (try . Byte.readFile) (fromBitString . Bit.bitString)
    ["-E", path] -> exec path (try . readFile) id
    ["-y", path] -> evalYolo path putStr humanifyExpression
    ['-' : _]    -> usage
    [path   ]    -> evalFile path putStr humanifyExpression
    _            -> usage
