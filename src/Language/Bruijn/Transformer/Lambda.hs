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
  AnnF _ (DefinitionF subName subTerm subSub subNext) -> do
    let sub'  = transformDefinition subName subTerm subSub subNext
    let term' = transformSub subNext term
    unFix $ flip foldFix term' $ \case
      AnnF _ (SubstitutionF name) | name == subName -> sub'
      t -> Fix t
  _ -> unFix term

transformNext :: Identifier -> TermAnn -> TermAnn -> TermAnn
transformNext name term next = flip foldFix next $ \case
  AnnF _ (SubstitutionF nextName) | nextName == name -> term
  t -> Fix t

-- via Abstraction Application: (!!)

-- subst sub (all next recursively) in term
-- subst term' in next (all next recursively)
transformDefinition :: Identifier -> TermAnn -> TermAnn -> TermAnn -> TermAnn
transformDefinition name term sub next = do
  let term' = transformSub sub term
  flip mapTermAnn next $ \case
    AnnF a EmptyF -> unFix term'
    _             -> unFix $ transformNext name term' next

transformSubstitution :: TermAnn -> TermAnn
transformSubstitution = mapTermAnn $ \case
  AnnF a (DefinitionF name term sub next) ->
    unFix $ transformDefinition name term sub next
  t -> t

transformTerm :: (MonadError m) => TermAnn -> m Lambda.TermAnn
transformTerm = foldFix $ \case
  AnnF a (AbstractionF term) -> Fix . AnnF a . Lambda.AbstractionF <$> term
  AnnF a (ApplicationF terms) ->
    Fix . AnnF a . Lambda.ApplicationF <$> sequenceA terms
  AnnF a (IndexF n) -> return $ Fix $ AnnF a $ Lambda.IndexF n

  -- this shall never surface (but still always gets transformed!)
  AnnF a EmptyF     -> return $ Fix $ AnnF a $ Lambda.IndexF (-1)

  -- else: throw error
  AnnF a termM ->
    sequenceA termM
      >>= throwError
      .   TransformError a
      .   ("unexpected " <>)
      .   T.pack
      .   show

-- | "main" is just a placeholder for whatever the last remaining function is
transformMain :: (MonadError m) => TermAnn -> m Lambda.TermAnn
transformMain (Fix (AnnF a (DefinitionF _ main _ _))) = transformTerm main
transformMain (Fix (AnnF a _)) =
  throwError $ TransformError a "expected main function"

-- TODO: the definitions tree has to be inversed first to preserve substitution order!
--       optimally, also make "main" head for easier filtering
transform :: (MonadError m) => TermAnn -> m Lambda.TermAnn
transform = transformMain . transformSubstitution
