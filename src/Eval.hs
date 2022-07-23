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
import           Debug.Trace
import           Helper
import           Parser
import           Paths_bruijn
import           Reducer
import           System.Console.Haskeline
import           System.Console.Haskeline.Completion
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath.Posix          ( takeBaseName )
import           System.IO
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
split a@(d : ds) b@(c : cs)
  | Just suffix <- a `stripPrefix` b = [] : split a suffix
  | otherwise = if null rest then [[c]] else (c : head rest) : tail rest
  where rest = split a $ tail b

-- TODO: Force naming convention for namespaces/files
loadFile :: String -> IO EnvState
loadFile path = do
  file <- try $ readFile path :: IO (Either IOError String)
  case file of
    Left exception -> print (exception :: IOError) >> pure (EnvState [])
    Right file ->
      eval (filter (not . null) $ split "\n\n" file) (EnvState []) False

evalVar :: String -> Program (Failable Expression)
evalVar var = state $ \e ->
  ( case lookup var (map fst e) of
    Nothing -> Left $ UndeclaredFunction var
    Just x  -> Right x
  , e
  )

evalApp :: Expression -> Expression -> Program (Failable Expression)
evalApp f g =
  evalExp f
    >>= (\case
          Left  e  -> pure $ Left e
          Right f' -> fmap (Application f') <$> evalExp g
        )

evalExp :: Expression -> Program (Failable Expression)
evalExp idx@(Bruijn      _  ) = pure $ Right idx
evalExp (    Variable    var) = evalVar var
evalExp (    Abstraction exp) = evalExp exp >>= pure . fmap Abstraction
evalExp (    Application f g) = evalApp f g

evalDefine :: String -> Expression -> [EnvDef] -> Program (Failable Expression)
evalDefine name exp sub =
  evalExp exp
    >>= (\case
          Left  e -> pure $ Left e
          Right f -> modify (((name, f), sub) :) >> pure (Right f)
        )

evalTest :: Expression -> Expression -> Program (Failable Instruction)
evalTest exp1 exp2 =
  evalExp exp1
    >>= (\case
          Left  exp1 -> pure $ Left exp1
          Right exp1 -> fmap (Test exp1) <$> evalExp exp2
        )

eval :: [String] -> EnvState -> Bool -> IO EnvState
eval []   state _ = return state
eval [""] state _ = return state
eval (block : bs) state@(EnvState env) isRepl =
  handleInterrupt (putStrLn "<aborted>" >> return state)
    $ case parse blockParser "" block of
        Left  err   -> putStrLn (errorBundlePretty err) >> eval bs state isRepl
        Right instr -> case instr of
          Define name exp sub ->
            -- TODO: sub: [Instruction] -> [EnvDef] (rec-mapping or sth?)
            let (res, env') = evalDefine name exp [] `runState` env
            in  case res of
                  Left err ->
                    putStrLn (show err) >> eval bs (EnvState env') isRepl
                  Right _ -> if isRepl
                    then (putStrLn $ name <> " = " <> show exp)
                      >> return (EnvState env')
                    else eval bs (EnvState env') isRepl
          -- TODO: Import loop detection
          -- TODO: Don't import subimports into main env
          Import path namespace -> do
            lib           <- getDataFileName path -- TODO: Use actual lib directory
            exists        <- doesFileExist lib
            EnvState env' <- loadFile $ if exists then lib else path
            let prefix | null namespace   = takeBaseName path ++ "."
                       | namespace == "." = ""
                       | otherwise        = namespace ++ "."
            env' <- pure $ map (\((n, e), s) -> ((prefix ++ n, e), s)) env'
            eval bs (EnvState $ env <> env') False -- import => isRepl = False
          Evaluate exp ->
            let (res, env') = evalExp exp `runState` env
            in
              putStrLn
                  (case res of
                    Left err -> show err
                    Right exp ->
                      "<> "
                        <> (show exp)
                        <> "\n*> "
                        <> (show reduced)
                        <> (if likeTernary reduced
                             then
                               "\t(" <> (show $ ternaryToDecimal reduced) <> ")"
                             else ""
                           )
                      where reduced = reduce exp
                  )
                >> eval bs state isRepl
          Test exp1 exp2 ->
            let (res, _) = evalTest exp1 exp2 `runState` env
            in  case res of
                  Left err -> putStrLn (show err) >> pure state
                  Right (Test exp1' exp2') ->
                    when
                        (reduce exp1' /= reduce exp2')
                        (  putStrLn
                        $  "ERROR: test failed: "
                        <> (show exp1)
                        <> " != "
                        <> (show exp2)
                        )
                      >> eval bs state isRepl
          _ -> eval bs state isRepl
  where blockParser = if isRepl then parseReplLine else parseBlock 0

evalFunc :: String -> Environment -> Maybe Expression
evalFunc func env = do
  exp <- lookup func $ map fst env
  pure $ reduce exp

evalMainFunc :: Environment -> Expression -> Maybe Expression
evalMainFunc env arg = do
  exp <- lookup "main" $ map fst env
  pure $ reduce $ Application exp arg

evalFile :: String -> (a -> IO ()) -> (Expression -> a) -> IO ()
evalFile path write conv = do
  EnvState env <- loadFile path
  arg          <- encodeStdin
  case evalMainFunc env arg of
    Nothing  -> putStrLn $ "ERROR: main function not found"
    Just exp -> write $ conv exp

exec :: String -> (String -> IO (Either IOError a)) -> (a -> String) -> IO ()
exec path read conv = do
  file <- read path
  case file of
    Left  exception -> print (exception :: IOError)
    Right file      -> print $ reduce $ fromBinary $ conv file

repl :: EnvState -> InputT M ()
repl state =
  (handleInterrupt (return $ Just "") $ withInterrupt $ getInputLine
      "\ESC[36mλ\ESC[0m "
    )
    >>= (\case
          Nothing   -> return ()
          Just line -> do
            state <- (liftIO $ eval [line] state True)
            lift (StrictState.put state)
            repl state
        )

lookupCompletion :: String -> M [Completion]
lookupCompletion str = do
  (EnvState env) <- StrictState.get
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
  let looper = runInputTWithPrefs prefs
                                  (completionSettings history)
                                  (withInterrupt $ repl $ EnvState [])
  code <- StrictState.evalStateT looper (EnvState [])
  return code

usage :: IO ()
usage = do
  putStrLn "Invalid arguments. Use 'bruijn [option] path' instead"
  putStrLn "-c\tcompile path to binary-BLC"
  putStrLn "-C\tcompile path to ASCII-BLC"
  putStrLn "-e\texecute path as binary-BLC"
  putStrLn "-E\texecute path as ASCII-BLC"
  putStrLn "-*\tshow this help"
  putStrLn "<default>\texecute path as text-bruijn"

evalMain :: IO ()
evalMain = do
  args <- getArgs
  case args of
    []           -> runRepl
    ["-c", path] -> evalFile path
                             (Byte.putStr . Bit.realizeBitStringStrict)
                             (toBitString . toBinary)
    ["-C", path] -> evalFile path putStrLn toBinary
    ["-e", path] ->
      exec path (try . Byte.readFile) (fromBitString . Bit.bitString)
    ["-E", path] -> exec path (try . readFile) id
    ['-' : _]    -> usage
    [path   ]    -> evalFile path print id
    _            -> usage
