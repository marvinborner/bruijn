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
                                                )
import           Data.Fix                       ( Fix(..) )
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
import           Language.Generic.Error         ( MonadError
                                                , throwError
                                                , Error(..)
                                                )

cata :: Functor f => (f a -> a) -> Fix f -> a
cata f (Fix x) = f (fmap (cata f) x)

-- TODO: this should transform to annotated term
--       we later have multiple reduction approaches, where most will strip the annotations beforehand
--       this will allow us to add really cool debugging features!
transform :: (MonadError m) => TermAnn -> m Lambda.TermAnn
transform = cata $ \case
  (AnnF a (DefinitionF name term sub next)) -> throwError $ TransformError a ""
  (AnnF a (PreprocessorF command sub next)) -> throwError $ TransformError a ""
  (AnnF a EmptyF                          ) -> throwError $ TransformError a ""

  (AnnF a (AbstractionF  term    )        ) -> Fix . AnnF a . Lambda.AbstractionF <$> term
  (AnnF a (ApplicationF  terms   )        ) -> throwError $ TransformError a ""
  (AnnF a (IndexF        n       )        ) -> throwError $ TransformError a ""

  (AnnF a (SubstitutionF name    )        ) -> throwError $ TransformError a ""
  (AnnF a (PrefixF name term     )        ) -> throwError $ TransformError a ""

  (AnnF a (SugarF sugar          )        ) -> throwError $ TransformError a "unexpected sugar"
  (AnnF a (TestF   left right    )        ) -> throwError $ TransformError a "unexpected test"
  (AnnF a (ImportF path namespace)        ) -> throwError $ TransformError a "unexpected import"
