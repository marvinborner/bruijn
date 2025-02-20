-- MIT License, Copyright (c) 2025 Marvin Borner
{-# LANGUAGE TemplateHaskell #-}

module Data.Bruijn
  ( TermF(..)
  , Term(..)
  , Identifier(..)
  , MixfixIdentifier(..)
  , SyntacticSugar(..)
  , Name
  ) where

import           Data.Fix                       ( Fix(..) )
import           Data.Text                      ( Text )
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

            deriving (Show, Eq, Functor)

deriveShow1 ''TermF

type Term = Fix TermF
