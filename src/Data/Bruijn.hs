-- MIT License, Copyright (c) 2025 Marvin Borner
{-# LANGUAGE TemplateHaskell #-}

module Data.Bruijn
  ( TermF(..)
  , Term(..)
  , Identifier(..)
  , MixfixIdentifier(..)
  , SyntacticSugar(..)
  , Name
  , TermAnn(..)
  , TermAnnF(..)
  , cata
  , mapIdentifiers
  ) where

import           Data.Fix                       ( Fix(..)
                                                , foldFix
                                                )
import           Data.Functor.Compose           ( getCompose )
import           Data.Text                      ( Text )
import           Language.Generic.Annotation    ( AnnF
                                                , pattern AnnF
                                                , AnnUnit(..)
                                                , SrcSpan
                                                )
import           Text.Show.Deriving             ( deriveShow1 )

type Name = Text

data SyntacticSugar = String Text
                      | UnaryNumber Integer
                      | BinaryNumber Integer
                      | BalancedTernaryNumber Integer
                      | Rational Integer Integer
                      | Real Integer Integer
                      | Complex Integer Integer
                      deriving (Show, Eq)

data MixfixIdentifier = Wildcard | Special Name
  deriving (Show, Eq)

data Identifier = Local Name | Namespaced Name Identifier | Mixfix [MixfixIdentifier]
  deriving (Show, Eq)

data TermF f = DefinitionF Identifier f f f -- | <name> <term> [<sub> <next>]
            | PreprocessorF f f f           -- | <command> [<sub> <next>]
            | EmptyF

            | AbstractionF f                -- | Abstraction of a term
            | ApplicationF [f]              -- | Left application of terms
            | IndexF Int                    -- | de Bruijn index
            | SubstitutionF Identifier      -- | Usage of a definition
            | PrefixF Identifier f          -- | Prefixed term
            | SugarF SyntacticSugar         -- | Syntactic sugar eventually desugared to Lambdas

            | TestF f f                     -- | :test (<lambda>) (<lambda>)
            | ImportF Text Text             -- | :import <path> <namespace>

            deriving (Show, Eq, Functor, Foldable, Traversable)

deriveShow1 ''TermF

type Term = Fix TermF

type TermAnnF = AnnF SrcSpan TermF
type TermAnn = Fix TermAnnF

-- to cata only on term: `cata (go . annotation . getCompose)`
cata :: Functor f => (f a -> a) -> Fix f -> a
cata f (Fix x) = f (fmap (cata f) x)

-- | Map all identifiers to a function
mapIdentifiers :: (Identifier -> Identifier) -> TermAnn -> TermAnn
mapIdentifiers func = foldFix $ \case
  (AnnF a (DefinitionF ident term sub next)) ->
    Fix $ AnnF a $ DefinitionF (func ident) term sub next
  (AnnF a (SubstitutionF ident)) -> Fix $ AnnF a $ SubstitutionF (func ident)
  t                              -> Fix t
