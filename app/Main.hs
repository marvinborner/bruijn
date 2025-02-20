module Main where

import qualified Data.Text                     as T
import           Language.Bruijn.Parser         ( parse )
import           Options.Applicative

main :: IO ()
main = do
  program <- getContents
  case parse (T.pack program) of
    Left  err -> putStrLn err
    Right ast -> print ast
