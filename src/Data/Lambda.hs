-- MIT License, Copyright (c) 2025 Marvin Borner
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Lambda (
  Term,
  TermF (..),
  TermAnnF,
  TermAnn,
  alphaEquivalent,
) where

import Data.Context (Context)
import Data.Fix (Fix (..))
import Data.Foreign (ForeignLanguage)
import Data.Text (Text)
import Language.Generic.Annotation (AnnF, pattern Ann)
import Text.Show.Deriving (deriveShow1)

data TermF f
  = -- | Abstraction of a term
    AbstractionF f
  | -- | Left application of terms
    ApplicationF [f]
  | -- | de Bruijn index
    IndexF Int
  | -- | Beta equality test
    TestF f f
  | -- | Execution token
    TokenF
  | -- | Execution token (returning)
    CotokenF
  | -- | ffi "..."
    ForeignF ForeignLanguage Text
  deriving
    ( Show
    , Eq
    , Functor
    )

deriveShow1 ''TermF

type Term = Fix TermF

type TermAnnF ph = AnnF (Context ph) TermF
type TermAnn ph = Fix (TermAnnF ph)

-- TODO: use cata or deriving Eq (couldn't make it work idk)
alphaEquivalent (Ann _ (AbstractionF a)) (Ann _ (AbstractionF b)) = a `alphaEquivalent` b
alphaEquivalent (Ann _ (ApplicationF a)) (Ann _ (ApplicationF b)) = and $ zipWith alphaEquivalent a b
alphaEquivalent (Ann _ (IndexF a)) (Ann _ (IndexF b)) = a == b
alphaEquivalent _ _ = False

-- alphaEquivalent (Ann _ a) (Ann _ b) = a == b
