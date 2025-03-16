-- MIT License, Copyright (c) 2025 Marvin Borner

module Language.Bruijn.PrettyPrinter
  ( prettyPrint
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

prettyPrint :: TermAnn -> Text
prettyPrint t = T.pack $ show $ flip foldFix t $ \case
  (AnnF a (DefinitionF name term sub next)) -> do
    let next' = if show next == "" then next else line <> next
    let sub' =
          if show sub == "" then indent tab sub else line <> indent tab sub
    hover (showAnnotationURI a) (prettyIdentifier name)
      <+> term
      <>  sub'
      <>  next'
  (AnnF a (PreprocessorF command sub next)) ->
    command <> line <> indent tab sub <> line <> next

  (AnnF a (AbstractionF  term  )) -> lbracket <> term <> rbracket
  (AnnF a (ApplicationF  [term])) -> term
  (AnnF a (ApplicationF  terms )) -> lbrace <> foldl1 (<>) terms <> rbrace
  (AnnF a (IndexF        n     )) -> int n

  (AnnF a (SubstitutionF name  )) -> prettyIdentifier name
  (AnnF a (PrefixF name term   )) -> prettyIdentifier name <> term

  (AnnF a EmptyF                ) -> empty

  (AnnF a (SugarF sugar)        ) -> text "SUGAR" -- TODO
  (AnnF a (TestF left right)) ->
    text ":test"
      <> space
      <> lparen
      <> left
      <> rparen
      <> space
      <> lparen
      <> right
      <> rparen
  (AnnF a (ImportF path namespace)) ->
    text ":import " <> text path <+> text namespace
