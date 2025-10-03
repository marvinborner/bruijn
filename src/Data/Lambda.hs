-- MIT License, Copyright (c) 2025 Marvin Borner
{-# LANGUAGE TemplateHaskell #-}

module Data.Lambda (
  Term,
  TermF (..),
  TermAnnF,
  TermAnn,
) where

import Data.Context (Context)
import Data.Fix (Fix (..))
import Language.Generic.Annotation (AnnF)
import Text.Show.Deriving (deriveShow1)

data TermF f
  = AbstractionF f
  | -- | Abstraction of a term
    ApplicationF [f]
  | -- | Left application of terms
    IndexF Int
  | -- | de Bruijn index
    TokenF
  | -- | Execution token
    CotokenF
  deriving (-- | Execution token (returning)
            Show, Eq, Functor)

deriveShow1 ''TermF

type Term = Fix TermF

type TermAnnF ph = AnnF (Context ph) TermF
type TermAnn ph = Fix (TermAnnF ph)
