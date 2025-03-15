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
                                                , mapIdentifiers
                                                )
import           Data.Fix                       ( Fix(..)
                                                , foldFix
                                                )
import           Data.List                      ( genericReplicate )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Text.IO.Utf8              ( readFile )
import           Language.Generic.Annotation    ( pattern AnnF, SrcSpan )
import           Language.Generic.Error         ( MonadError
                                                , throwError
                                                , Error(..)
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
  maybeContents <- liftIO
    $ try @IOException $ readFile $ T.unpack $ file
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
  (AnnF ann (SugarF sugar)) -> pure $ desugar ann sugar
  (AnnF ann (ImportF path namespace)) -> importPath ann process path namespace
  t -> Fix <$> sequenceA t
