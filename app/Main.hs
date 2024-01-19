module Main where

import           Eval
import           Helper                         ( ArgMode(..)
                                                , Args(..)
                                                )
import           Options.Applicative

mode :: Parser ArgMode
mode =
  flag' ArgEvalBblc
        (long "eval-bblc" <> short 'e' <> help "Evaluate file with BLC bits")
    <|> flag'
          ArgEvalBlc
          (long "eval-blc" <> short 'E' <> help "Evaluate file with ASCII BLC")
    <|> flag' ArgDumpBblc
              (long "dump-bblc" <> short 'b' <> help "Dump file as BLC bits")
    <|> flag' ArgDumpBlc
              (long "dump-blc" <> short 'B' <> help "Dump file as ASCII BLC")

args :: Parser Args
args =
  Args
    <$> (mode <|> pure ArgEval)
    <*> switch (long "yolo" <> short 'y' <> help "Don't run tests")
    <*> strOption
          (long "target" <> short 't' <> metavar "TARGET" <> value "" <> help
            "Optimize to target using BLoC and BLoCade"
          )
    <*> optional (argument str (metavar "PATH" <> help "Path to file"))

main :: IO ()
main = evalMain =<< execParser opts
 where
  opts =
    info (args <**> helper) (fullDesc <> header "bruijn programming language")
