-- MIT License, Copyright (c) 2025 Marvin Borner

module Data.Foreign (
  ForeignLanguage (..),
) where

data ForeignLanguage = Bruijn | Haskell | UXN | AMD64
  deriving (Show, Eq)
