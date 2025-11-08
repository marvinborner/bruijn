-- MIT License, Copyright (c) 2025 Marvin Borner
{-# LANGUAGE TemplateHaskell #-}

module Data.Bruijn (
  TermF (..),
  Term,
  Identifier (..),
  MixfixIdentifier (..),
  SyntacticSugar (..),
  IntegerEncoding (..),
  FloatingEncoding (..),
  Name,
  TermAnn,
  TermAnnF,
  mapIdentifiers,
  linkIn,
) where

import Data.Context (Context (..))
import Data.Fix (Fix (..))
import Data.Foreign
import Data.Functor.Foldable (cata)
import Data.Text (Text)
import Language.Generic.Annotation (
  AnnF,
  mapAlgebra,
  pattern Ann,
  pattern AnnF,
 )
import Text.Show.Deriving (deriveShow1)

type Name = Text

data IntegerEncoding
  = Unary
  | Binary
  | BalancedTernary
  | DeBruijn
  | BuiltinUnsigned
  | BuiltinSigned
  deriving (Show, Eq)
data FloatingEncoding = Rational | Real | Complex | BuiltinDouble
  deriving (Show, Eq)

data SyntacticSugar
  = String Text
  | IntegerNumber IntegerEncoding Integer
  | FloatingNumber FloatingEncoding Integer
  deriving (Show, Eq)

data MixfixIdentifier = Wildcard | Special Name
  deriving (Show, Eq)

data Identifier
  = Local Name
  | Namespaced Name Identifier
  | Mixfix [MixfixIdentifier]
  | Prefix Name
  deriving (Show, Eq)

-- data TermTypeF f = Singleton (TermF f) | Product (TermF f) (TermF f)

-- data SignatureF f = TypeName Text | FunctionType [SignatureF] | ConstructorType

data TermF f
  = -- | <name> <term> [<sub> <next>]
    DefinitionF Identifier f f f
  | -- | <command> [<sub> <next>]
    PreprocessorF f f f
  | -- | do <term> [<sub> <next>]
    DoF f f f
  | EmptyF
  | -- | Abstraction of a term
    AbstractionF f
  | -- | Left application of terms
    ApplicationF [f]
  | -- | de Bruijn index
    IndexF Int
  | -- | Usage of a definition
    SubstitutionF Identifier
  | -- | Prefixed term
    PrefixF Identifier f
  | -- | Syntactic sugar eventually desugared to Lambdas
    SugarF SyntacticSugar
  | -- | Force execution by token
    ForceF
  | -- | :test (<lambda>) (<lambda>)
    TestF f f
  | -- | :import <path> <namespace>
    ImportF Text Text
  | -- | ffi "..."
    ForeignF ForeignLanguage Text
  deriving
    ( Show
    , Eq
    , Functor
    , Foldable
    , Traversable
    )

deriveShow1 ''TermF

type Term = Fix TermF

type TermAnnF ph = AnnF (Context ph) TermF
type TermAnn ph = Fix (TermAnnF ph)

-- | Map all identifiers to a function
mapIdentifiers :: (Identifier -> Identifier) -> TermAnn c -> TermAnn c
mapIdentifiers func = cata $ \case
  (AnnF a (DefinitionF ident term sub next)) ->
    Fix $ AnnF a $ DefinitionF (func ident) term sub next
  (AnnF a (SubstitutionF ident)) -> Fix $ AnnF a $ SubstitutionF (func ident)
  t -> Fix t

-- | Link term into next-chain
linkIn :: TermAnn c -> TermAnn c -> TermAnn c
linkIn term subst = flip mapAlgebra term $ \case
  DefinitionF name body sub (Ann _ EmptyF) ->
    DefinitionF name body sub subst
  DefinitionF name body sub next ->
    DefinitionF name body sub $ linkIn next subst
  PreprocessorF command sub (Ann _ EmptyF) ->
    PreprocessorF command sub subst
  PreprocessorF command sub next ->
    PreprocessorF command sub $ linkIn next subst
  DoF body sub (Ann _ EmptyF) ->
    DoF body sub subst
  DoF body sub next ->
    DoF body sub $ linkIn next subst
  t -> t
