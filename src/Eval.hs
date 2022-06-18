module Eval
  ( evalMain
  ) where

import           Binary
import           Control.Exception
import           Control.Monad.State
import qualified Data.BitString                as Bit
import qualified Data.ByteString               as Byte
import           Debug.Trace
import           Helper
import           Parser
import           Reducer
import           System.Console.Haskeline
import           System.Environment
import           System.Exit
import           System.IO
import           Text.Megaparsec         hiding ( State
                                                , try
                                                )

loadFile :: String -> IO Environment
loadFile path = do
  file <- try $ readFile path :: IO (Either IOError String)
  case file of
    Left  exception -> print (exception :: IOError) >> pure []
    Right file      -> eval (filter (not . null) $ lines file) [] False

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

eval :: [String] -> Environment -> Bool -> IO Environment
eval []          env _      = pure env
eval (line : ls) env isRepl = case parse lineParser "BRUIJN" line of
  Left  err   -> putStrLn (errorBundlePretty err) >> pure env
  Right instr -> case instr of
    Define name exp ->
      let (res, env') = evalDefine name exp `runState` env
      in  case res of
            Left  err -> putStrLn (show err) >> eval ls env' isRepl
            Right _   -> if isRepl
              then (putStrLn $ name <> " = " <> show exp) >> pure env'
              else eval ls env' isRepl
    Import path -> loadFile path
    Evaluate exp ->
      let (res, env') = evalExp exp `runState` env
      in  putStrLn
              (case res of
                Left err -> show err
                Right exp ->
                  "<> "
                    <> (show exp)
                    <> "\n*> "
                    <> (show reduced)
                    <> "\t("
                    <> (show $ binaryToDecimal reduced)
                    <> ")"
                  where reduced = reduce exp
              )
            >> eval ls env isRepl
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
                >> eval ls env isRepl
    _ -> eval ls env isRepl
  where lineParser = if isRepl then parseReplLine else parseLine

evalFunc :: String -> Environment -> Maybe Expression
evalFunc func env = do
  exp <- lookup func env
  pure $ reduce exp

repl :: Environment -> InputT IO ()
repl env =
  getInputLine "λ "
    >>= (\case
          Nothing   -> pure ()
          Just line -> (lift $ eval [line] env True) >>= repl
        )

evalFile :: String -> (a -> IO ()) -> (Expression -> a) -> IO ()
evalFile path write conv = do
  env <- loadFile path
  case evalFunc "main" env of
    Nothing  -> putStrLn $ "ERROR: main function not found"
    Just exp -> write $ conv exp

exec :: String -> (String -> IO (Either IOError a)) -> (a -> String) -> IO ()
exec path read conv = do
  file <- read path
  case file of
    Left  exception -> print (exception :: IOError)
    Right file      -> print $ reduce $ fromBinary $ conv file

usage :: IO ()
usage = putStrLn "Invalid arguments. Use 'bruijn [file]' instead"

evalMain :: IO ()
evalMain = do
  args <- getArgs
  case args of
    [] -> runInputT defaultSettings { historyFile = Just ".bruijn-history" }
      $ repl []
    ["-c", path] -> evalFile path
                             (Byte.putStr . Bit.realizeBitStringStrict)
                             (toBitString . toBinary)
    ["-C", path] -> evalFile path putStrLn toBinary
    ["-e", path] ->
      exec path (try . Byte.readFile) (fromBitString . Bit.bitString)
    ["-E", path] -> exec path (try . readFile) id
    ['-' : _]    -> usage
    [path   ]    -> evalFile path print id
