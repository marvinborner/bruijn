module Eval
  ( evalMain
  ) where

import           Control.Exception
import           Control.Monad.State
import           Debug.Trace
import           Helper
import           Parser
import           Reducer
import           System.Console.Haskeline
import           System.Environment
import           System.Exit
import           System.IO
import           Text.Parsec             hiding ( State
                                                , try
                                                )

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

evalTest :: Expression -> Expression -> Program (Failable Instruction)
evalTest exp1 exp2 =
  evalExp exp1
    >>= (\case
          Left  exp1 -> pure $ Left exp1
          Right exp1 -> fmap (Test exp1) <$> evalExp exp2
        )

eval :: [String] -> Environment -> IO Environment
eval []          env = pure env
eval (line : ls) env = case parse parseLine "FILE" line of
  Left  err   -> print err >> pure env
  Right instr -> case instr of
    Define name exp ->
      let (res, env') = evalDefine name exp `runState` env
      in  case res of
            Left  err -> print err >> eval ls env'
            Right _   -> eval ls env'
    Test exp1 exp2 ->
      let (res, _) = evalTest exp1 exp2 `runState` env
      in  case res of
            Left err -> putStrLn (show err) >> pure env
            Right (Test exp1' exp2') ->
              when
                  (reduce exp1' /= reduce exp2')
                  (  putStrLn
                  $  "ERROR: test failed: "
                  <> (show exp1)
                  <> " != "
                  <> (show exp2)
                  )
                >> eval ls env
    _ -> eval ls env

evalFunc :: String -> Environment -> IO Environment
evalFunc func env = case lookup func env of
  Nothing  -> (putStrLn $ func <> " not found") >> pure env
  Just exp -> (print $ reduce exp) >> pure env

-- TODO: Less duplicate code (liftIO?)
-- TODO: Generally improve eval code
evalRepl :: String -> Environment -> InputT IO Environment
evalRepl line env = case parse parseReplLine "REPL" line of
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
                Left err -> show err
                Right exp ->
                  "<> "
                    <> (show exp)
                    <> "\n*> "
                    <> (show reduced)
                    <> "\t("
                    <> (show $ ternaryToDecimal reduced)
                    <> ")"
                  where reduced = reduce exp
              )
            >> pure env
    Load path ->
      liftIO
        $   (try $ readFile path :: IO (Either IOError String))
        >>= (\case -- TODO: Make this more abstract and reusable
              Left exception -> print (exception :: IOError) >> pure env
              Right file -> eval (filter (not . null) $ lines file) [] >>= pure
            )
    Test exp1 exp2 ->
      let (res, _) = evalTest exp1 exp2 `runState` env
      in  case res of
            Left err -> outputStrLn (show err) >> pure env
            Right (Test exp1' exp2') ->
              when
                  (reduce exp1' /= reduce exp2')
                  (  outputStrLn
                  $  "ERROR: test failed: "
                  <> (show exp1)
                  <> " != "
                  <> (show exp2)
                  )
                >> pure env
    _ -> pure env

evalFile :: String -> IO ()
evalFile path = do
  file <- try $ readFile path :: IO (Either IOError String)
  case file of
    Left exception -> print (exception :: IOError)
    Right file ->
      eval (filter (not . null) $ lines file) []
        >>= evalFunc "main"
        >>  return ()

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
