-- MIT License, Copyright (c) 2025 Marvin Borner
-- TODO: desugar terms
-- TODO: substitute open terms directly (see Transformer.Lambda)
-- TODO: filter foreign calls depending on system/flag
{-# LANGUAGE TypeApplications #-}

module Language.Bruijn.Preprocessor (
  preprocess,
) where

import Control.Exception (
  IOException,
  try,
 )
import Control.Monad.IO.Class (
  MonadIO,
  liftIO,
 )
import Data.Bruijn (
  Identifier (..),
  IntegerEncoding (..),
  Name,
  SyntacticSugar (..),
  TermAnn,
  TermF (..),
  linkIn,
  mapIdentifiers,
 )
import Data.Context (Context (..), phaseChange)
import Data.Fix (Fix (..), foldFix)
import Data.List (genericReplicate)
import Data.Phase (Phase (BruijnParse, BruijnPreprocess))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO.Utf8 (readFile)
import Language.Generic.Annotation (
  pattern Ann,
  pattern AnnF,
 )
import Language.Generic.Error (
  Error (..),
  MonadError,
  PhaseT (..),
  runPhaseTOrFail,
  throwError,
 )
import Prelude hiding (readFile)

type SourceTerm = TermAnn BruijnParse
type Term = TermAnn BruijnPreprocess
type PhaseContext = Context BruijnPreprocess
type PhaseError = MonadError (Error BruijnPreprocess)

-- | Desugar syntactic sugar to equivalent term
desugar :: PhaseContext -> SyntacticSugar -> Term
desugar context (IntegerNumber Unary n) =
  f
    ( AbstractionF
        ( f
            ( AbstractionF
                ( foldr
                    (\h t -> f (ApplicationF [h, t]))
                    (f (IndexF 1))
                    (genericReplicate n (f (IndexF 0)))
                )
            )
        )
    )
 where
  f = Fix . AnnF context
desugar _ _ = error "not implemented yet"

-- | Desugar import to linked definitions
importPath ::
  (MonadIO m, PhaseError m) =>
  PhaseContext ->
  (Text -> Text -> PhaseT m Term) ->
  Text ->
  Name ->
  m Term
importPath context process path namespace = do
  -- TODO: also search in std
  let file = path <> ".bruijn"
  maybeContents <- liftIO $ try @IOException $ readFile $ T.unpack file
  case maybeContents of
    Left err -> throwError $ Error context $ T.pack $ show err
    Right contents -> do
      processed <- runPhaseTOrFail context $ process file contents
      return $ mapIdentifiers (Namespaced namespace) processed

preprocess ::
  (MonadIO m, PhaseError m) =>
  (Text -> Text -> PhaseT m Term) ->
  SourceTerm ->
  m Term
preprocess process = foldFix $ \case
  AnnF a (SugarF sugar) -> pure $ desugar (phaseChange a) sugar
  AnnF _ (PreprocessorF command sub next) ->
    command >>= \case
      Ann a (ImportF path namespace) -> do
        imported <- importPath a process path namespace
        linkIn imported <$> next
      c -> return c
  AnnF a t -> Fix . AnnF (phaseChange a) <$> sequence t
