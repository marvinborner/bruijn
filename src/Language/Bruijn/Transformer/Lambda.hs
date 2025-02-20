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
import           Data.Fix                       ( Fix(..)
                                                , foldFix
                                                )
import           Data.Functor.Compose           ( getCompose )
import qualified Data.Lambda                   as Lambda
                                                ( Term(..)
                                                , TermF(..)
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Language.Generic.Annotation    ( AnnUnit(..) )
import           Language.Generic.Error         ( ErrorOr
                                                , throwError
                                                )

cata :: Functor f => (f a -> a) -> Fix f -> a
cata f (Fix x) = f (fmap (cata f) x)

transFix :: TermF f -> ErrorOr Lambda.Term
transFix (DefinitionF name term sub next) = throwError ""
transFix (PreprocessorF command sub next) = throwError ""
transFix EmptyF                           = throwError ""

transFix (AbstractionF  term    )         = throwError ""
transFix (ApplicationF  terms   )         = throwError ""
transFix (IndexF        n       )         = throwError ""

transFix (SubstitutionF name    )         = throwError ""
transFix (PrefixF name term     )         = throwError ""
transFix (SugarF sugar          )         = throwError ""

transFix (TestF   left right    )         = throwError ""
transFix (ImportF path namespace)         = throwError ""

transform :: TermAnn -> ErrorOr Lambda.Term
transform = cata (transFix . annotated . getCompose)
