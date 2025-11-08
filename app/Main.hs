-- MIT License, Copyright (c) 2025 Marvin Borner
{-# LANGUAGE DataKinds #-}

module Main where

import Control.Monad.IO.Class (
  MonadIO,
  liftIO,
 )
import qualified Data.Bruijn as Bruijn (
  TermAnn,
 )
import qualified Data.Lambda as Lambda (
  TermAnn,
 )
import Data.Phase (Phase (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO.Utf8 (putStrLn)
import qualified Language.Bruijn.Parser as Bruijn (
  parse,
 )
import qualified Language.Bruijn.Preprocessor as Bruijn (
  preprocess,
 )
import qualified Language.Bruijn.PrettyPrinter as Bruijn (
  prettyPrintAnnotated,
 )
import qualified Language.Bruijn.Transformer.Lambda as BruijnToLambda (
  transform,
 )
import Language.Generic.Error (
  MonadError,
  PhaseT,
  liftPhase,
  runExceptT,
  runPhaseT,
 )
import qualified Language.Lambda.PrettyPrinter as Lambda (
  prettyPrintAnnotated,
 )
import qualified Language.Lambda.Reducer.RKNL as Lambda (
  reduce,
 )
import Prelude hiding (putStrLn)

bruijnPipeline ::
  (MonadIO m) => Text -> Text -> PhaseT m (Bruijn.TermAnn BruijnPreprocess)
bruijnPipeline file input = do
  parsed <- liftPhase $ Bruijn.parse file input
  liftPhase $ Bruijn.preprocess bruijnPipeline parsed

pipeline ::
  (MonadIO m) => Text -> Text -> PhaseT m (Lambda.TermAnn LambdaReduce)
pipeline file input = do
  bruijn <- bruijnPipeline file input
  liftIO $ putStrLn "PARSED:"
  liftIO $ putStrLn $ Bruijn.prettyPrintAnnotated bruijn
  lambda <- liftPhase $ BruijnToLambda.transform bruijn
  liftIO $ putStrLn "\nTRANSFORMED:"
  liftIO $ putStrLn $ Lambda.prettyPrintAnnotated lambda
  liftIO $ putStrLn "\nREDUCED:"
  reduced <- liftPhase $ Lambda.reduce lambda
  -- let reduced = Lambda.reduce lambda
  liftIO $ putStrLn $ Lambda.prettyPrintAnnotated reduced
  return reduced

main :: IO ()
main = do
  program <- getContents
  res <- runExceptT $ runPhaseT $ pipeline "stdin" $ T.pack program
  case res of
    Left err -> putStrLn err
    Right ast -> do
      putStrLn "done"

-- print ast
