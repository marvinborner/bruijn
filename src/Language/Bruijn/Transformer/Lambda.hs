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
                                                , TermF(..)
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Language.Generic.Annotation    ( AnnUnit(..) )
import           Language.Generic.Error         ( MonadError
                                                , throwError
                                                )

cata :: Functor f => (f a -> a) -> Fix f -> a
cata f (Fix x) = f (fmap (cata f) x)

transform :: (MonadError Text m) => TermAnn -> m Lambda.Term
transform = cata (go . annotated . getCompose)
 where
  go :: (MonadError Text m) => TermF f -> m Lambda.Term
  go (DefinitionF name term sub next) = throwError ""
  go (PreprocessorF command sub next) = throwError ""
  go EmptyF                           = throwError ""

  go (AbstractionF  term    )         = throwError ""
  go (ApplicationF  terms   )         = throwError ""
  go (IndexF        n       )         = throwError ""

  go (SubstitutionF name    )         = throwError ""
  go (PrefixF name term     )         = throwError ""

  go (SugarF sugar          )         = throwError ""
  go (TestF   left right    )         = throwError "unexpected test"
  go (ImportF path namespace)         = throwError "unexpected import"
