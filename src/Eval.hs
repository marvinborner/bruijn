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
  { _env :: Environment
  }
type M = StrictState.StateT EnvState IO

-- why isn't this in Prelude??
split :: (Eq a) => [a] -> [a] -> [[a]]
split _  [] = []
split [] x  = map (: []) x
split a@(_ : _) b@(c : _)
  | Just suffix <- a `stripPrefix` b = [] : split a suffix
  | otherwise = if null rest then [[c]] else (c : head rest) : tail rest
  where rest = split a $ tail b

-- TODO: Force naming convention for namespaces/files
loadFile :: String -> EvalConf -> IO EnvState
loadFile path conf = do
  f <- try $ readFile path :: IO (Either IOError String)
  case f of
    Left exception ->
      print (ContextualError (ImportError $ show (exception :: IOError)) (Context "" (nicePath conf))) >> pure (EnvState $ Environment [])
    Right f' -> eval (filter (not . null) $ split "\n\n" f')
                       (EnvState $ Environment [])
                       (conf { isRepl = False, evalPaths = (path : (evalPaths conf)) })

evalVar :: String -> Environment -> Program (Failable Expression)
evalVar var (Environment sub) = state $ \env@(Environment e) ->
  let lookup' name env' = case lookup name env' of
        Nothing -> Left $ UndeclaredFunction var
        Just x  -> Right x
  in  case lookup' var (map fst sub) of -- search in sub env
        s@(Right _) -> (s, env)
        _           -> (lookup' var (map fst e), env) -- search in global env

evalAbs :: Expression -> Environment -> Program (Failable Expression)
evalAbs e sub = evalExp e sub >>= pure . fmap Abstraction

evalApp :: Expression -> Expression -> Environment -> Program (Failable Expression)
evalApp f g sub =
  evalExp f sub
    >>= (\case
          Left  e  -> pure $ Left e
          Right f' -> fmap (Application f') <$> evalExp g sub
        )

evalInfix :: Expression -> String -> Expression -> Environment -> Program (Failable Expression)
evalInfix le i re = evalExp $ Application (Application (Variable i) le) re

evalExp :: Expression -> Environment -> Program (Failable Expression)
evalExp idx@(Bruijn      _  ) = const $ pure $ Right idx
evalExp (    Variable    var) = evalVar var
evalExp (    Abstraction e) = evalAbs e
evalExp (    Application f g) = evalApp f g
evalExp (Infix le i re) = evalInfix le i re

evalDefine
  :: String -> Expression -> Environment -> Program (Failable Expression)
evalDefine name e sub =
  evalExp e sub
    >>= (\case
          Left e' -> pure $ Left e'
          Right f ->
            modify (\(Environment s) -> Environment $ ((name, f), Environment []) : s)
              >> pure (Right f)
        )

evalTest :: Expression -> Expression -> Environment -> Program (Failable Instruction)
evalTest e1 e2 sub =
  evalExp e1 sub
    >>= (\case
          Left  err -> pure $ Left err
          Right e1' -> fmap (Test e1') <$> evalExp e2 sub
        )

evalSubEnv :: [Instruction] -> EnvState -> EvalConf -> IO EnvState
evalSubEnv [] s _ = return s
evalSubEnv (instr : is) s conf =
  handleInterrupt (putStrLn "<aborted>" >> return s)
    $ evalInstruction instr s (evalSubEnv is) conf

evalInstruction
  :: Instruction
  -> EnvState
  -> (EnvState -> EvalConf -> IO EnvState)
  -> EvalConf
  -> IO EnvState
evalInstruction (ContextualInstruction instr inp) s@(EnvState env) rec conf = case instr of
  Define name e sub -> do
    EnvState subEnv <- evalSubEnv sub s conf
    let
      (res, env') = evalDefine name e subEnv `runState` env
     in  case res of
          Left  err -> print (ContextualError err (Context inp (nicePath conf))) >> pure s -- don't continue
          Right _   -> if isRepl conf
            then (putStrLn $ name <> " = " <> show e)
              >> return (EnvState env')
            else rec (EnvState env') conf
  -- TODO: Don't import subimports into main env
  Import path namespace -> do
    lib           <- getDataFileName path -- TODO: Use actual lib directory
    exists        <- doesFileExist lib
    actual        <- pure $ if exists then lib else path
    if (actual `elem` evalPaths conf) then (print (ContextualError (ImportError path) (Context inp (nicePath conf))) >> pure s) else do
      EnvState env' <- loadFile actual (conf { nicePath = path }) -- TODO: Fix wrong `within` in import error
      let prefix | null namespace   = takeBaseName path ++ "."
                 | namespace == "." = ""
                 | otherwise        = namespace ++ "."
      env'' <- pure $ Environment $ map (\((n, e), o) -> ((prefix ++ n, e), o))
                                       ((\(Environment e) -> e) env') -- TODO: Improve
      rec (EnvState $ env'' <> env) (conf { isRepl = False, evalPaths = evalPaths conf }) -- import => isRepl = False
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
                  <> " "
                  <> (humanifyExpression reduced)
                where reduced = reduce e'
            )
          >> rec s conf
  Test e1 e2 -> if (evalTests conf) then
    let (res, _) = evalTest e1 e2 (Environment []) `runState` env
    in  case res of
          Left err -> print (ContextualError err (Context inp (nicePath conf))) >> pure s
          Right (Test e1' e2') ->
            when
                (lhs /= rhs)
                (print $ FailedTest e1 e2 lhs rhs)
              >> rec s conf
            where
              lhs = reduce e1'
              rhs = reduce e2'
          _ -> rec s conf
    else rec s conf
  _ -> rec s conf
evalInstruction instr s rec conf = evalInstruction (ContextualInstruction instr "<unknown>") s rec conf

eval :: [String] -> EnvState -> EvalConf -> IO EnvState
eval []   s _ = return s
eval [""] s _ = return s
eval (block : bs) s conf =
  handleInterrupt (putStrLn "<aborted>" >> return s)
    $ case parse blockParser "" block of
        Left  err   -> print (ContextualError (SyntaxError $ printBundle err) (Context "" (nicePath conf))) >> eval bs s conf
        Right instr -> evalInstruction instr s (eval bs) conf
  where blockParser = if isRepl conf then parseReplLine else parseBlock 0

evalMainFunc :: Environment -> Expression -> Maybe Expression
evalMainFunc (Environment env) arg = do
  e <- lookup "main" (map fst env)
  pure $ reduce $ Application e arg

evalFileConf :: String -> (a -> IO ()) -> (Expression -> a) -> EvalConf -> IO ()
evalFileConf path wr conv conf = do
  EnvState env <- loadFile path conf
  arg          <- encodeStdin
  case evalMainFunc env arg of
    Nothing  -> print $ ContextualError (UndeclaredFunction "main") (Context "" path)
    Just e -> wr $ conv e

evalFile :: String -> (a -> IO ()) -> (Expression -> a) -> IO ()
evalFile path wr conv = evalFileConf path wr conv (EvalConf { isRepl = False, evalTests = True, nicePath = path, evalPaths = [] })

evalYolo :: String -> (a -> IO ()) -> (Expression -> a) -> IO ()
evalYolo path wr conv = evalFileConf path wr conv (EvalConf { isRepl = False, evalTests = False, nicePath = path, evalPaths = [] })

exec :: String -> (String -> IO (Either IOError a)) -> (a -> String) -> IO ()
exec path rd conv = do
  f <- rd path
  case f of
    Left  exception -> print (exception :: IOError)
    Right f'      -> print $ reduce $ fromBinary $ conv f'

repl :: EnvState -> InputT M ()
repl s =
  (handleInterrupt (return $ Just "") $ withInterrupt $ getInputLine
      "\ESC[36mλ\ESC[0m "
    )
    >>= (\case -- TODO: Add non-parser error support for REPL
          Nothing   -> return ()
          Just line -> do -- TODO: Use -y in repl for YOLO lifestyle
            s' <- (liftIO $ eval [line] s (EvalConf { isRepl = True, evalTests = True, nicePath = "<repl>", evalPaths = [] }))
            lift (StrictState.put s')
            repl s'
        )

lookupCompletion :: String -> M [Completion]
lookupCompletion str = do
  (EnvState (Environment env)) <- StrictState.get
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
  let looper = runInputTWithPrefs
        prefs
        (completionSettings history)
        (withInterrupt $ repl $ EnvState $ Environment [])
  code <- StrictState.evalStateT looper (EnvState $ Environment [])
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
    ["-b", path] -> evalFile path
                             (Byte.putStr . Bit.realizeBitStringStrict)
                             (toBitString . toBinary)
    ["-B", path] -> evalFile path putStrLn toBinary
    ["-e", path] ->
      exec path (try . Byte.readFile) (fromBitString . Bit.bitString)
    ["-E", path] -> exec path (try . readFile) id
    ["-y", path] -> evalYolo path putStr humanifyExpression
    ['-' : _]    -> usage
    [path   ]    -> evalFile path putStr humanifyExpression
    _            -> usage
