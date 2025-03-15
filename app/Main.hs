-- MIT License, Copyright (c) 2025 Marvin Borner

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
import           Data.Text.IO.Utf8              ( putStrLn )
import qualified Language.Bruijn.Parser        as Bruijn
                                                ( parse )
import qualified Language.Bruijn.Preprocessor  as Bruijn
                                                ( preprocess )
import qualified Language.Bruijn.Tracer        as Bruijn
                                                ( traceAnn )
import qualified Language.Bruijn.Transformer.Lambda
                                               as Bruijn2Lambda
                                                ( transform )
import           Language.Generic.Error         ( MonadError
                                                , runErrorT
                                                , showError
                                                )
import           Prelude                 hiding ( putStrLn )

bruijnPipeline :: (MonadIO m, MonadError m) => Text -> Text -> m Bruijn.TermAnn
bruijnPipeline file input = do
  bruijn <- Bruijn.parse file input
  -- TODO: Bruijn.test
  -- Bruijn.traceAnn (Bruijn.preprocess bruijnPipeline) bruijn
  Bruijn.preprocess bruijnPipeline bruijn

-- pipeline :: (MonadIO m, MonadError m) => Text -> Text -> m Lambda.Term
pipeline file input = do
  bruijn <- bruijnPipeline file input
  liftIO $ print bruijn
  lambda <- Bruijn2Lambda.transform bruijn
  return lambda

main :: IO ()
main = do
  program <- getContents
  res     <- runErrorT $ pipeline "stdin" (T.pack program)
  case res of
    Left err -> do
      pretty <- showError err
      putStrLn pretty
    Right ast -> print ast
