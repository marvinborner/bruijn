-- MIT License, Copyright (c) 2025 Marvin Borner
{-# LANGUAGE TemplateHaskell #-}

module Data.Lambda
  ( Term(..)
  , TermF(..)
  ) where

import           Data.Fix                       ( Fix(..) )
import           Text.Show.Deriving             ( deriveShow1 )

data TermF f = AbstractionF f  -- | Abstraction of a term
            | ApplicationF [f] -- | Left application of terms
            | IndexF Int       -- | de Bruijn index
            deriving (Show, Eq, Functor)

deriveShow1 ''TermF

type Term = Fix TermF
