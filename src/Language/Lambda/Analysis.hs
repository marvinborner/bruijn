-- MIT License, Copyright (c) 2025 Marvin Borner

module Language.Lambda.Analysis (
  isOpen,
) where

import Data.Lambda (TermF (..))

isOpen :: TermF f -> Boolean

-- isOpen = go 0
--   where go n (AbstractionF term) =
