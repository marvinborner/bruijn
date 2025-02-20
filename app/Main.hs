module Main where

import qualified Data.Lambda                   as Lambda
                                                ( Term )
import qualified Data.Text                     as T
import           Data.Text                      ( Text )
import qualified Language.Bruijn.Parser        as Bruijn
                                                ( parse )
import qualified Language.Bruijn.Transformer.Lambda
                                               as Bruijn2Lambda
                                                ( transform )
import           Language.Generic.Error         ( ErrorOr
                                                , runError
                                                )
import           Options.Applicative

pipeline :: Text -> ErrorOr Lambda.Term
pipeline p = do
  bruijn <- Bruijn.parse p
  lambda <- Bruijn2Lambda.transform bruijn
  return lambda

main :: IO ()
main = do
  program <- getContents
  case runError $ pipeline (T.pack program) of
    Left  err -> putStrLn $ "error: " <> T.unpack err
    Right ast -> print ast
