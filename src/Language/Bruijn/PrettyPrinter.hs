-- MIT License, Copyright (c) 2025 Marvin Borner

module Language.Bruijn.PrettyPrinter
  ( prettyPrint
  ) where

import           Data.Fix                       ( foldFix )

prettyPrint :: TermAnn -> Text
prettyPrint _ = ""
