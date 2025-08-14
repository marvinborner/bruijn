-- MIT License, Copyright (c) 2025 Marvin Borner
{-# LANGUAGE TemplateHaskell #-}

module Data.Lambda
  ( Term
  , TermF(..)
  , TermAnnF
  , TermAnn
  ) where

import           Data.Fix                       ( Fix(..) )
import           Language.Generic.Annotation    ( AnnF
                                                , SrcSpan
                                                )
import           Text.Show.Deriving             ( deriveShow1 )

data TermF f = AbstractionF f  -- | Abstraction of a term
            | ApplicationF [f] -- | Left application of terms
            | IndexF Int       -- | de Bruijn index
            | TokenF           -- | Execution token
            | CotokenF         -- | Execution token (returning)
            deriving (Show, Eq, Functor)

deriveShow1 ''TermF

type Term = Fix TermF

type TermAnnF = AnnF SrcSpan TermF
type TermAnn = Fix TermAnnF
