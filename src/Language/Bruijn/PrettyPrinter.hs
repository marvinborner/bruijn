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
import           Language.Generic.PrettyPrinter ( hover
                                                , text
                                                )
import           Text.PrettyPrint.Leijen hiding ( text )

tab :: Int
tab = 4

prettyMixfixIdentifier :: MixfixIdentifier -> Doc
prettyMixfixIdentifier = \case
  Wildcard     -> char '…'
  Special name -> text name

prettyIdentifier :: Identifier -> Doc
prettyIdentifier = \case
  Local name                 -> text name
  Namespaced "."  identifier -> prettyIdentifier identifier
  Namespaced name identifier -> text name <> dot <> prettyIdentifier identifier
  Mixfix identifiers -> foldl1 (<>) (fmap prettyMixfixIdentifier identifiers)

prettyPrintAlgebra :: TermF Doc -> Doc
prettyPrintAlgebra = \case
  DefinitionF name term sub next -> do -- TODO: deduplicate
    let sub' =
          if show sub == "" then indent tab sub else line <> indent tab sub
    let next' = if show next == "" then next else line <> next
    prettyIdentifier name <+> term <> sub' <> next'
  PreprocessorF command sub next -> do
    let sub' =
          if show sub == "" then indent tab sub else line <> indent tab sub
    let next' = if show next == "" then next else line <> next
    command <> sub' <> next'
  DoF term sub next -> do
    let sub' =
          if show sub == "" then indent tab sub else line <> indent tab sub
    let next' = if show next == "" then next else line <> next
    text "do " <> term <> sub' <> next'
  AbstractionF  term   -> lbracket <> term <> rbracket
  ApplicationF  [term] -> term
  ApplicationF  terms  -> lparen <> foldl1 ((<>) . (<> space)) terms <> rparen
  IndexF        n      -> int n
  SubstitutionF name   -> prettyIdentifier name
  PrefixF name term    -> prettyIdentifier name <> term
  EmptyF               -> empty
  SugarF sugar         -> text "SUGAR" -- TODO
  ForceF               -> text "!"
  TestF left right ->
    text ":test"
      <> space
      <> lparen
      <> left
      <> rparen
      <> space
      <> lparen
      <> right
      <> rparen
  ImportF path namespace -> text ":import " <> text path <+> text namespace

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
