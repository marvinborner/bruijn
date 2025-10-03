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
  , linkIn
  ) where

import           Data.Fix                       ( Fix(..)
                                                , foldFix
                                                )
import           Data.Functor.Compose           ( getCompose )
import           Data.Text                      ( Text )
import           Language.Generic.Annotation    ( AnnF
                                                , pattern AnnF
                                                , pattern FixAnnF
                                                , AnnUnit(..)
                                                , mapAlgebra
                                                )
import Data.Context (Context(..))
import           Text.Show.Deriving             ( deriveShow1 )
import Data.Foreign

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
            | DoF f f f                     -- | do <term> [<sub> <next>]
            | EmptyF

            | AbstractionF f                -- | Abstraction of a term
            | ApplicationF [f]              -- | Left application of terms
            | IndexF Int                    -- | de Bruijn index
            | SubstitutionF Identifier      -- | Usage of a definition
            | PrefixF Identifier f          -- | Prefixed term
            | SugarF SyntacticSugar         -- | Syntactic sugar eventually desugared to Lambdas

            | ForceF                        -- | Force execution by token

            | TestF f f                     -- | :test (<lambda>) (<lambda>)
            | ImportF Text Text             -- | :import <path> <namespace>

            | Foreign ForeignLanguage Text  -- | ffi "..."
            | ForeignIf ForeignLanguage f   -- | <term>@<lang> (filtered by preprocessor)

            deriving (Show, Eq, Functor, Foldable, Traversable)

deriveShow1 ''TermF

type Term = Fix TermF

type TermAnnF ph = AnnF (Context ph) TermF
type TermAnn ph = Fix (TermAnnF ph)

-- | Map all identifiers to a function
mapIdentifiers :: (Identifier -> Identifier) -> TermAnn c -> TermAnn c
mapIdentifiers func = foldFix $ \case
  (AnnF a (DefinitionF ident term sub next)) ->
    Fix $ AnnF a $ DefinitionF (func ident) term sub next
  (AnnF a (SubstitutionF ident)) -> Fix $ AnnF a $ SubstitutionF (func ident)
  t                              -> Fix t

-- | Link term into next-chain
linkIn :: TermAnn c -> TermAnn c -> TermAnn c
linkIn term subst = flip mapAlgebra term $ \case
  DefinitionF name term sub (FixAnnF _ EmptyF) ->
    DefinitionF name term sub subst
  DefinitionF name term sub next ->
    DefinitionF name term sub $ linkIn next subst
  PreprocessorF command sub (FixAnnF _ EmptyF) ->
    PreprocessorF command sub subst
  PreprocessorF command sub next ->
    PreprocessorF command sub $ linkIn next subst
  DoF term sub (FixAnnF _ EmptyF) ->
    DoF term sub subst
  DoF term sub next ->
    DoF term sub $ linkIn next subst
  t -> t
