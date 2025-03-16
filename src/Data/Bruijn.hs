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
  , mapIdentifiers
  , mapTermAnn
  , mapTermAlgebra
  , linkIn
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

-- to foldFix only on term: `foldFix (go . annotation . getCompose)`

-- mapTermAnn :: (TermAnnF TermAnn -> TermAnnF TermAnn) -> TermAnn -> TermAnn
mapTermAnn f (Fix termAnnF) = Fix (f termAnnF)

mapTermAlgebra f = mapTermAnn $ \case
  AnnF a inn -> AnnF a (f inn)

mapAnn f (Fix (AnnF a termF)) = Fix (AnnF (f a) termF)

-- | Map all identifiers to a function
mapIdentifiers :: (Identifier -> Identifier) -> TermAnn -> TermAnn
mapIdentifiers func = foldFix $ \case
  (AnnF a (DefinitionF ident term sub next)) ->
    Fix $ AnnF a $ DefinitionF (func ident) term sub next
  (AnnF a (SubstitutionF ident)) -> Fix $ AnnF a $ SubstitutionF (func ident)
  t                              -> Fix t

-- | Link term into next-chain
linkIn :: TermAnn -> TermAnn -> TermAnn
linkIn term subst = flip mapTermAlgebra term $ \case
  DefinitionF name term sub (Fix (AnnF _ EmptyF)) ->
    DefinitionF name term sub subst
  DefinitionF name term sub next ->
    DefinitionF name term sub $ linkIn next subst
  PreprocessorF command sub (Fix (AnnF _ EmptyF)) ->
    PreprocessorF command sub subst
  PreprocessorF command sub next ->
    PreprocessorF command sub $ linkIn next subst
  t -> t
