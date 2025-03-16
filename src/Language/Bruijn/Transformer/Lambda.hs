-- MIT License, Copyright (c) 2025 Marvin Borner

module Language.Bruijn.Transformer.Lambda
  ( transform
  ) where

import           Control.Monad                  ( (<=<) )
import           Data.Bruijn                    ( Identifier(..)
                                                , SyntacticSugar(..)
                                                , TermAnn(..)
                                                , TermAnnF(..)
                                                , TermF(..)
                                                , mapTermAnn
                                                )
import           Data.Fix                       ( Fix(..)
                                                , foldFix
                                                , unFix
                                                )
import           Data.Functor.Compose           ( getCompose )
import qualified Data.Lambda                   as Lambda
                                                ( Term(..)
                                                , TermAnn(..)
                                                , TermAnnF(..)
                                                , TermF(..)
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Language.Generic.Annotation    ( AnnUnit(..)
                                                , pattern AnnF
                                                )
import           Language.Generic.Error         ( Error(..)
                                                , MonadError
                                                , throwError
                                                )

-- TODO: should we move sub/next to a Substitutor module (for phases like tests etc.)?

-- TODO: this should use outermost Abstraction Application via shift (trivial!)
transformSub :: TermAnn -> TermAnn -> TermAnn
transformSub sub term = flip mapTermAnn sub $ \case
  (AnnF _ (DefinitionF subName subTerm subSub subNext)) -> do
    let sub' = transformDefinition subName subTerm subSub subNext
    let term' = transformSub subNext term
    unFix $ flip foldFix term' $ \case
      (AnnF _ (SubstitutionF name)) | name == subName -> sub'
      t -> Fix t
  _ -> unFix term

transformNext :: Identifier -> TermAnn -> TermAnn -> TermAnn
transformNext name term next = flip foldFix next $ \case
  (AnnF _ (SubstitutionF nextName)) | nextName == name -> term
  t -> Fix t

-- via Abstraction Application: (!!)

-- subst sub (all next recursively) in term
-- subst term' in next (all next recursively)
-- transformDefinition :: ... -> TermAnn
transformDefinition name term sub next = do
  let term' = transformSub sub term
  transformNext name term' next

transformSubstitution :: TermAnn -> TermAnn
transformSubstitution = foldFix $ \case
  (AnnF a (DefinitionF name term sub next)) ->
    transformDefinition name term sub next
  t -> Fix t

transform :: (MonadError m) => TermAnn -> m Lambda.TermAnn
transform term = do
  -- TODO: the definitions tree has to be inversed first to preserve substitution order!
  --       optimally, also make "main" head for easier filtering
  let term' = transformSubstitution term
  flip foldFix term' $ \case
    (AnnF a (DefinitionF name term sub next)) ->
      throwError $ TransformError a "definition"

    (AnnF a (AbstractionF term)) -> Fix . AnnF a . Lambda.AbstractionF <$> term
    (AnnF a (ApplicationF  terms)) -> throwError $ TransformError a "app"
    (AnnF a (IndexF        n    )) -> throwError $ TransformError a "idx"

    (AnnF a (SubstitutionF name )) -> throwError $ TransformError a "subst"
    (AnnF a (PrefixF name term  )) -> throwError $ TransformError a "prefix"

    -- this should never surface
    (AnnF a EmptyF               ) -> pure $ Fix $ AnnF a $ Lambda.IndexF (-1)

    (AnnF a (PreprocessorF{})) ->
      throwError $ TransformError a "unexpected preprocessor"
    (AnnF a (SugarF{} )) -> throwError $ TransformError a "unexpected sugar"
    (AnnF a (TestF{}  )) -> throwError $ TransformError a "unexpected test"
    (AnnF a (ImportF{})) -> throwError $ TransformError a "unexpected import"
