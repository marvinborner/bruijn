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
    <*> switch (long "verbose" <> short 'v' <> help "Increase verbosity")
    <*> switch
          (long "optimize" <> short 'O' <> help
            "Optimize program (abstraction of duplicated terms)"
          )
    <*> strOption
          (long "target" <> short 't' <> metavar "TARGET" <> value "" <> help
            "Compile to target using BLoC and BLoCade"
          )
    <*> strOption
          (  long "reducer"
          <> short 'r'
          <> metavar "REDUCER"
          <> value "HigherOrder"
          <> help "Reducer (currently RKNL, ION, or HigherOrder)"
          )
    <*> optional (argument str (metavar "PATH" <> help "Path to file"))

main :: IO ()
main = evalMain =<< execParser opts
 where
  opts =
    info (args <**> helper) (fullDesc <> header "bruijn programming language")
