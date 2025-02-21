-- MIT License, Copyright (c) 2025 Marvin Borner

module Language.Bruijn.Preprocessor
  ( preprocess
  ) where

import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Data.Bruijn                    ( Identifier(..)
                                                , Name
                                                , SyntacticSugar(..)
                                                , TermAnn
                                                , TermAnnF
                                                , TermF(..)
                                                , cata
                                                )
import           Data.Fix                       ( Fix(..)
                                                , foldFix
                                                )
import           Data.List                      ( genericReplicate )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Text.IO                   ( readFile )
import           Language.Generic.Annotation    ( pattern AnnF, SrcSpan )
import           Language.Generic.Error         ( MonadError
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

-- | Map all identifiers to a function
mapIdentifiers :: (Identifier -> Identifier) -> TermAnn -> TermAnn
mapIdentifiers func = foldFix $ \case
  (AnnF a (DefinitionF ident term sub next)) ->
    Fix $ AnnF a $ DefinitionF (func ident) term sub next
  (AnnF a (SubstitutionF ident)) -> Fix $ AnnF a $ SubstitutionF (func ident)
  t                              -> Fix t

-- | Desugar import to linked definitions
importPath
  :: (MonadIO m, MonadError Text m)
  => (Text -> m TermAnn)
  -> Text
  -> Name
  -> m TermAnn
importPath process path namespace = do
  contents <- liftIO $ readFile $ T.unpack path -- TODO: amend path
  mapIdentifiers (Namespaced namespace) <$> process contents

preprocess
  :: (MonadIO m, MonadError Text m)
  => (Text -> m TermAnn)
  -> TermAnn
  -> m TermAnn
preprocess process = foldFix $ \case
  (AnnF ann (SugarF sugar)) -> pure (desugar ann sugar)
  (AnnF _ (ImportF path namespace)) -> importPath process path namespace
  t -> Fix <$> sequenceA t
