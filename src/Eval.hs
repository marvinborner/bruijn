module Eval
  ( evalMain
  ) where

import           Control.Exception
import           Control.Monad.State
import           Debug.Trace
import           Parser
import           Reducer
import           System.Console.Haskeline
import           System.Environment
import           System.Exit
import           System.IO
import           Text.Parsec             hiding ( State
                                                , try
                                                )

type Environment = [(String, Expression)]
type Program = State Environment

evalVar :: String -> Program (Failable Expression)
evalVar var = state $ \e ->
  ( case lookup var e of
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

-- TODO: Duplicate function error
evalDefine :: String -> Expression -> Program (Failable Expression)
evalDefine name exp =
  evalExp exp
    >>= (\case
          Left  e -> pure $ Left e
          Right f -> modify ((name, f) :) >> pure (Right f)
        )

eval :: [String] -> Environment -> IO Environment
eval []          env = pure env
eval (line : ls) env = case parse parseLine "Evaluator" line of
  Left  err   -> print err >> pure env
  Right instr -> case instr of
    Define name exp ->
      let (res, env') = evalDefine name exp `runState` env
      in  case res of
            Left  err -> print err >> eval ls env'
            Right _   -> (putStrLn $ name <> " = " <> show exp) >> eval ls env'
    Evaluate exp ->
      let (res, env') = evalExp exp `runState` env
      in  putStrLn
              (case res of
                Left  err -> show err
                Right exp -> show $ reduce exp
              )
            >> pure env
    _ -> eval ls env

-- TODO: Less duplicate code (liftIO?)
-- TODO: Convert back to my notation using custom show
evalRepl :: String -> Environment -> InputT IO Environment
evalRepl line env = case parse parseReplLine "Repl" line of
  Left  err   -> outputStrLn (show err) >> pure env
  Right instr -> case instr of
    Define name exp ->
      let (res, env') = evalDefine name exp `runState` env
      in  case res of
            Left  err -> outputStrLn (show err) >> pure env'
            Right _   -> (outputStrLn $ name <> " = " <> show exp) >> pure env'
    Evaluate exp ->
      let (res, env') = evalExp exp `runState` env
      in  outputStrLn
              (case res of
                Left  err -> show err
                Right exp -> (show exp) <> "\n-> " <> (show $ reduce exp)
              )
            >> pure env
    _ -> pure env

evalFile :: String -> IO ()
evalFile path = do
  file <- try $ readFile path :: IO (Either IOError String)
  case file of
    Left  exception -> print (exception :: IOError)
    Right file      -> eval (lines file) [] >> putStrLn "Done"

repl :: Environment -> InputT IO ()
repl env =
  getInputLine "λ "
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
    [] -> runInputT defaultSettings { historyFile = Just ".bruijn-history" }
      $ repl []
    [path] -> evalFile path
    _      -> usage
