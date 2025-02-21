module Main where

import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import qualified Data.Bruijn                   as Bruijn
                                                ( TermAnn )
import qualified Data.Lambda                   as Lambda
                                                ( Term )
import qualified Data.Text                     as T
import           Data.Text                      ( Text )
import qualified Language.Bruijn.Parser        as Bruijn
                                                ( parse )
import qualified Language.Bruijn.Preprocessor  as Bruijn
                                                ( preprocess )
import qualified Language.Bruijn.Transformer.Lambda
                                               as Bruijn2Lambda
                                                ( transform )
import           Language.Generic.Error         ( MonadError
                                                , runExceptT
                                                )

bruijnPipeline :: (MonadIO m, MonadError Text m) => Text -> m Bruijn.TermAnn
bruijnPipeline p = do
  bruijn <- Bruijn.parse p
  -- TODO: Bruijn.test
  Bruijn.preprocess bruijnPipeline bruijn

pipeline :: (MonadIO m, MonadError Text m) => Text -> m Lambda.Term
pipeline p = do
  bruijn <- bruijnPipeline p
  liftIO $ print bruijn
  lambda <- Bruijn2Lambda.transform bruijn
  return lambda

main :: IO ()
main = do
  program <- getContents
  res     <- runExceptT $ pipeline (T.pack program)
  case res of
    Left  err -> putStrLn $ "error: " <> T.unpack err
    Right ast -> print ast
