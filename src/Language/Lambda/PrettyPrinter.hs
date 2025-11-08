-- MIT License, Copyright (c) 2025 Marvin Borner

module Language.Lambda.PrettyPrinter (
  prettyPrint,
  prettyPrintAnnotated,
) where

import Data.Context (Context (..))
import Data.Functor.Compose (getCompose)
import Data.Functor.Foldable (cata)
import Data.Lambda (
  TermAnn,
  TermF (..),
 )
import Data.Text (Text)
import qualified Data.Text as T
import Language.Generic.Annotation (
  AnnUnit (..),
  showAnnotationURI,
  pattern AnnF,
 )
import Language.Generic.PrettyPrinter (
  hover,
  text,
 )
import Text.PrettyPrint.Leijen hiding (text)

prettyPrintAlgebra :: TermF Doc -> Doc
prettyPrintAlgebra = \case
  AbstractionF term -> lbracket <> term <> rbracket
  ApplicationF [term] -> term
  ApplicationF terms -> lparen <> foldl1 ((<>) . (<> space)) terms <> rparen
  TestF left right -> lparen <> left <> text " =β= " <> right <> rparen
  IndexF n -> int n
  TokenF -> text "<"
  CotokenF -> text ">"
  ForeignF lang source -> text "ffi@" <> text (T.pack $ show lang) <+> text source

-- | purely textual pretty printing
prettyPrint :: TermAnn ph -> Text
prettyPrint =
  T.pack . show . cata (prettyPrintAlgebra . annotated . getCompose)

{- | pretty print with hovering (uses terminal escape sequences)
TODO: also enable coloring here
-}
prettyPrintAnnotated :: TermAnn ph -> Text
prettyPrintAnnotated = T.pack . show . cata go
 where
  go (AnnF (Context{_srcSpan = a}) t) = hover (showAnnotationURI a) (prettyPrintAlgebra t)
