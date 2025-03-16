-- MIT License, Copyright (c) 2025 Marvin Borner
{-# LANGUAGE TypeApplications #-}

module Language.Bruijn.Preprocessor
  ( preprocess
  ) where

import           Control.Exception              ( IOException
                                                , try
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Data.Bruijn                    ( Identifier(..)
                                                , Name
                                                , SyntacticSugar(..)
                                                , TermAnn
                                                , TermF(..)
                                                , linkIn
                                                , mapIdentifiers
                                                )
import           Data.Fix                       ( Fix(..), foldFix )
import           Data.List                      ( genericReplicate )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Text.IO.Utf8              ( readFile )
import           Language.Generic.Annotation    ( pattern AnnF, SrcSpan )
import           Language.Generic.Error         ( Error(..)
                                                , MonadError
                                                , throwError
                                                )
import           Prelude                 hiding ( readFile )

-- | Desugar syntactic sugar to equivalent term
desugar :: SrcSpan -> SyntacticSugar -> TermAnn
desugar ann (UnaryNumber n) = f
  (AbstractionF
    (f
      (AbstractionF
        (foldr (\h t -> f (ApplicationF [h, t]))
               (f (IndexF 1))
               (genericReplicate n (f (IndexF 0)))
        )
      )
    )
  )
  where f = Fix . AnnF ann
desugar _ _ = error "not implemented yet"

-- | Desugar import to linked definitions
importPath
  :: (MonadIO m, MonadError m)
  => SrcSpan
  -> (Text -> Text -> m TermAnn)
  -> Text
  -> Name
  -> m TermAnn
importPath ann process path namespace = do
  -- TODO: also search in std
  let file = path <> ".bruijn"
  maybeContents <- liftIO $ try @IOException $ readFile $ T.unpack $ file
  case maybeContents of
    Left err -> throwError $ PreprocessError ann $ T.pack $ show err
    Right contents ->
      mapIdentifiers (Namespaced namespace) <$> process file contents

preprocess
  :: (MonadIO m, MonadError m)
  => (Text -> Text -> m TermAnn)
  -> TermAnn
  -> m TermAnn
preprocess process = foldFix $ \case
  AnnF a (SugarF sugar) -> pure $ desugar a sugar
  AnnF _ (PreprocessorF command sub next) -> command >>= \case
      Fix (AnnF a (ImportF path namespace)) -> do
        imported <- importPath a process path namespace
        next' <- next
        return $ linkIn imported next'
      c -> return c
  t -> Fix <$> sequenceA t
