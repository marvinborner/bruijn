module Eval
  ( evalMain
  ) where

import           Control.Exception
import           Control.Monad.State
import           System.Console.Haskeline
import           System.Environment
import           System.Exit
import           System.IO

type Environment = [String]

eval :: String -> IO ()
eval code = putStrLn "ok"

evalFile :: String -> IO ()
evalFile path = do
  file <- try $ readFile path :: IO (Either IOError String)
  case file of
    Left  exception -> print (exception :: IOError)
    Right file      -> eval file

evalRepl :: String -> Environment -> InputT IO Environment
evalRepl line env = outputStrLn (show env) >> pure env

repl :: Environment -> InputT IO ()
repl env =
  getInputLine ":: "
    >>= (\case
          Nothing   -> pure ()
          Just line -> evalRepl line env >>= repl
        )

usage :: IO ()
usage = putStrLn "Invalid arguments. Use 'bruijn [file]' instead"

evalMain :: IO ()
evalMain = do
  args <- getArgs
  case args of
    [] -> runInputT defaultSettings { historyFile = Just ".brown-history" }
      $ repl []
    [path] -> evalFile path
    _      -> usage
