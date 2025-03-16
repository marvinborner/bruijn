-- MIT License, Copyright (c) 2025 Marvin Borner

module Language.Bruijn.PrettyPrinter
  ( prettyPrint
  , prettyPrintAnnotated
  ) where

import           Data.Bruijn                    ( Identifier(..)
                                                , MixfixIdentifier(..)
                                                , TermAnn
                                                , TermF(..)
                                                )
import           Data.Fix                       ( foldFix )
import           Data.Functor.Compose           ( getCompose )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Language.Generic.Annotation    ( AnnUnit(..)
                                                , pattern AnnF
                                                , showAnnotationURI
                                                )
import           Text.PrettyPrint.Leijen hiding ( text )
import qualified Text.PrettyPrint.Leijen       as PP

tab :: Int
tab = 4

hover :: Text -> Doc -> Doc
hover note doc =
  string "\ESC]8;;" <> text note <> string "\ESC\\" <> doc <> string
    "\ESC]8;;\ESC\\"

text :: Text -> Doc
text = PP.text . T.unpack

prettyMixfixIdentifier :: MixfixIdentifier -> Doc
prettyMixfixIdentifier Wildcard       = char '…'
prettyMixfixIdentifier (Special name) = text name

prettyIdentifier :: Identifier -> Doc
prettyIdentifier (Local name               ) = text name
prettyIdentifier (Namespaced "." identifier) = prettyIdentifier identifier
prettyIdentifier (Namespaced name identifier) =
  text name <> dot <> prettyIdentifier identifier
prettyIdentifier (Mixfix identifiers) =
  foldl1 (<>) (fmap prettyMixfixIdentifier identifiers)

prettyPrintAlgebra :: TermF Doc -> Doc
prettyPrintAlgebra (DefinitionF name term sub next) = do
  let sub'  = if show sub == "" then indent tab sub else line <> indent tab sub
  let next' = if show next == "" then next else line <> next
  prettyIdentifier name <+> term <> sub' <> next'
prettyPrintAlgebra (PreprocessorF command sub next) = do
  let sub'  = if show sub == "" then indent tab sub else line <> indent tab sub
  let next' = if show next == "" then next else line <> next
  command <> sub' <> next'
prettyPrintAlgebra (AbstractionF  term  ) = lbracket <> term <> rbracket
prettyPrintAlgebra (ApplicationF  [term]) = term
prettyPrintAlgebra (ApplicationF terms) = lbrace <> foldl1 (<>) terms <> rbrace
prettyPrintAlgebra (IndexF        n     ) = int n
prettyPrintAlgebra (SubstitutionF name  ) = prettyIdentifier name
prettyPrintAlgebra (PrefixF name term   ) = prettyIdentifier name <> term
prettyPrintAlgebra (EmptyF              ) = empty
prettyPrintAlgebra (SugarF sugar        ) = text "SUGAR" -- TODO
prettyPrintAlgebra (TestF left right) =
  text ":test"
    <> space
    <> lparen
    <> left
    <> rparen
    <> space
    <> lparen
    <> right
    <> rparen
prettyPrintAlgebra (ImportF path namespace) =
  text ":import " <> text path <+> text namespace

-- | purely textual pretty printing
prettyPrint :: TermAnn -> Text
prettyPrint =
  T.pack . show . foldFix (prettyPrintAlgebra . annotated . getCompose)

-- | pretty print with hovering (uses terminal escape sequences)
-- TODO: also enable coloring here
prettyPrintAnnotated :: TermAnn -> Text
prettyPrintAnnotated = T.pack . show . foldFix go
 where
  go (AnnF _ EmptyF) = empty -- for linebreaks in if-check above
  go (AnnF a t     ) = hover (showAnnotationURI a) (prettyPrintAlgebra t)
