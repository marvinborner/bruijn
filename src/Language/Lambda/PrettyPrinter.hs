-- MIT License, Copyright (c) 2025 Marvin Borner

module Language.Lambda.PrettyPrinter
  ( prettyPrint
  , prettyPrintAnnotated
  ) where

import           Data.Fix                       ( foldFix )
import           Data.Functor.Compose           ( getCompose )
import           Data.Lambda                    ( TermAnn
                                                , TermF(..)
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import Data.Context (Context(..))
import           Language.Generic.Annotation    ( AnnUnit(..)
                                                , pattern AnnF
                                                , showAnnotationURI
                                                )
import           Language.Generic.PrettyPrinter ( hover )
import           Text.PrettyPrint.Leijen

prettyPrintAlgebra :: TermF Doc -> Doc
prettyPrintAlgebra = \case
  AbstractionF term   -> lbracket <> term <> rbracket
  ApplicationF [term] -> term
  ApplicationF terms  -> lparen <> foldl1 ((<>) . (<> space)) terms <> rparen
  IndexF       n      -> int n
  TokenF              -> text "<"
  CotokenF            -> text ">"

-- | purely textual pretty printing
prettyPrint :: TermAnn ph -> Text
prettyPrint =
  T.pack . show . foldFix (prettyPrintAlgebra . annotated . getCompose)

-- | pretty print with hovering (uses terminal escape sequences)
-- TODO: also enable coloring here
prettyPrintAnnotated :: TermAnn ph -> Text
prettyPrintAnnotated = T.pack . show . foldFix go
  where go (AnnF a t) = hover (showAnnotationURI (srcSpan a)) (prettyPrintAlgebra t)
