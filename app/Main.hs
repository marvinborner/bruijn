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
import qualified Language.Bruijn.PrettyPrinter as Bruijn
                                                ( prettyPrintAnnotated )
import qualified Language.Bruijn.Tracer        as Bruijn
                                                ( traceAnn )
import qualified Language.Bruijn.Transformer.Lambda
                                               as Bruijn2Lambda
                                                ( transform )
import           Language.Generic.Error         ( MonadError
                                                , runErrorT
                                                , showError
                                                )
import qualified Language.Lambda.PrettyPrinter as Lambda
                                                ( prettyPrintAnnotated )
-- import qualified Language.Lambda.Reducer.HigherOrder
--                                                as Lambda
--                                                 ( reduce )
import qualified Language.Lambda.Reducer.RKNL  as Lambda
                                                ( reduce )
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
  liftIO $ putStrLn "PARSED:"
  liftIO $ putStrLn $ Bruijn.prettyPrintAnnotated bruijn
  lambda <- Bruijn2Lambda.transform bruijn
  liftIO $ putStrLn "\nTRANSFORMED:"
  liftIO $ putStrLn $ Lambda.prettyPrintAnnotated lambda
  liftIO $ putStrLn "\nREDUCED:"
  reduced <- Lambda.reduce lambda
  liftIO $ putStrLn $ Lambda.prettyPrintAnnotated reduced
  return lambda

main :: IO ()
main = do
  program <- getContents
  res     <- runErrorT $ pipeline "stdin" $ T.pack program
  case res of
    Left err -> do
      pretty <- showError err
      putStrLn pretty
    Right ast -> do
      putStrLn "done"
      -- print ast
