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
loadFile :: String -> IO EnvState
loadFile path = do
  f <- try $ readFile path :: IO (Either IOError String)
  case f of
    Left exception ->
      print (exception :: IOError) >> pure (EnvState $ Environment [])
    Right f' -> eval (filter (not . null) $ split "\n\n" f')
                       (EnvState $ Environment [])
                       False

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

evalExp :: Expression -> Environment -> Program (Failable Expression)
evalExp idx@(Bruijn      _  ) = const $ pure $ Right idx
evalExp (    Variable    var) = evalVar var
evalExp (    Abstraction e) = evalAbs e
evalExp (    Application f g) = evalApp f g

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

evalSubEnv :: [Instruction] -> EnvState -> Bool -> IO EnvState
evalSubEnv [] s _ = return s
evalSubEnv (instr : is) s isRepl =
  handleInterrupt (putStrLn "<aborted>" >> return s)
    $ evalInstruction instr s (evalSubEnv is) isRepl

evalInstruction
  :: Instruction
  -> EnvState
  -> (EnvState -> Bool -> IO EnvState)
  -> Bool
  -> IO EnvState
evalInstruction instr s@(EnvState env) rec isRepl = case instr of
  Define name e sub inp -> do
    EnvState subEnv <- evalSubEnv sub s isRepl
    let
      (res, env') = evalDefine name e subEnv `runState` env
     in  case res of
          Left  err -> print (ContextualError err inp) >> pure s -- don't continue
          Right _   -> if isRepl
            then (putStrLn $ name <> " = " <> show e)
              >> return (EnvState env')
            else rec (EnvState env') isRepl
  -- TODO: Import loop detection
  -- TODO: Don't import subimports into main env
  Import path namespace -> do
    lib           <- getDataFileName path -- TODO: Use actual lib directory
    exists        <- doesFileExist lib
    EnvState env' <- loadFile $ if exists then lib else path
    let prefix | null namespace   = takeBaseName path ++ "."
               | namespace == "." = ""
               | otherwise        = namespace ++ "."
    env'' <- pure $ Environment $ map (\((n, e), o) -> ((prefix ++ n, e), o))
                                     ((\(Environment e) -> e) env') -- TODO: Improve
    rec (EnvState $ env'' <> env) False -- import => isRepl = False
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
                  <> (if likeTernary reduced
                       then "\t(" <> (show $ ternaryToDecimal reduced) <> ")"
                       else ""
                     )
                where reduced = reduce e'
            )
          >> rec s isRepl
  Test e1 e2 ->
    let (res, _) = evalTest e1 e2 (Environment []) `runState` env
    in  case res of
          Left err -> print err >> pure s
          Right (Test e1' e2') ->
            when
                (lhs /= rhs)
                (print $ FailedTest e1 e2 lhs rhs)
              >> rec s isRepl
            where
              lhs = reduce e1'
              rhs = reduce e2'
          _ -> rec s isRepl
  _ -> rec s isRepl

eval :: [String] -> EnvState -> Bool -> IO EnvState
eval []   s _ = return s
eval [""] s _ = return s
eval (block : bs) s isRepl =
  handleInterrupt (putStrLn "<aborted>" >> return s)
    $ case parse blockParser "" block of
        Left  err   -> print (SyntaxError $ errorBundlePretty err) >> eval bs s isRepl
        Right instr -> evalInstruction instr s (eval bs) isRepl
  where blockParser = if isRepl then parseReplLine else parseBlock 0

evalMainFunc :: Environment -> Expression -> Maybe Expression
evalMainFunc (Environment env) arg = do
  e <- lookup "main" (map fst env)
  pure $ reduce $ Application e arg

evalFile :: String -> (a -> IO ()) -> (Expression -> a) -> IO ()
evalFile path wr conv = do
  EnvState env <- loadFile path
  arg          <- encodeStdin
  case evalMainFunc env arg of
    Nothing  -> print $ ContextualError (UndeclaredFunction "main") path
    Just e -> wr $ conv e

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
          Just line -> do
            s' <- (liftIO $ eval [line] s True)
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
  putStrLn "-*\tshow this help"
  putStrLn "<default>\texecute path as text-bruijn"

evalMain :: IO ()
evalMain = do
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
    ['-' : _]    -> usage
    [path   ]    -> evalFile path print id
    _            -> usage
