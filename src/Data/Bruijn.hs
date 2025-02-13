-- MIT License, Copyright (c) 2025 Marvin Borner

module Data.Bruijn
  ( Term(..)
  , Lambda(..)
  , Identifier(..)
  , MixfixIdentifier(..)
  , SyntacticSugar(..)
  , Command(..)
  , Name
  ) where

import           Data.Text                      ( Text )

type Name = Text

data Command = Test Lambda Lambda -- | :test (<lambda>) (<lambda>)
               | Import Text Text           -- | :import <path> <namespace>
               deriving Show

data SyntacticSugar = String Text
                      | UnaryNumber Integer
                      | BinaryNumber Integer
                      | BalancedTernaryNumber Integer
                      | Rational Integer Integer
                      | Real Integer Integer
                      | Complex Integer Integer
                      deriving Show

data MixfixIdentifier = Wildcard | Special Name deriving Show

data Identifier = Local Name | Namespaced Name Identifier | Mixfix [MixfixIdentifier] deriving Show

data Lambda = Abstraction Lambda         -- | Abstractions of a term
              | Application [Lambda]     -- | Left application of terms
              | Index Int                -- | de Bruijn index
              | Substitution Identifier  -- | Usage of a definition
              | Prefix Identifier Lambda -- | Prefixed term
              | Sugar SyntacticSugar     -- | Syntactic sugar eventually desugared to Lambdas
              deriving Show

data Term = Definition Identifier Lambda Term Term -- | <name> <term> [<sub> <next>]
            | Preprocessor Command Term Term       -- | <command> [<sub> <next>]
            | Empty
            deriving Show
