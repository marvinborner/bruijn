-- MIT License, Copyright (c) 2025 Marvin Borner
-- for handling shared context across phases & languages elegantly
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Context (
  Context (..),
  SrcSpan (..),
  MetaLevel (..),
  phaseChange,
) where

import Data.Foreign (ForeignLanguage)
import Data.Phase
import GHC.Generics
import Text.Megaparsec (SourcePos (..))

data SrcSpan = SrcSpan
  { _spanBegin :: SourcePos
  , _spanEnd :: SourcePos
  }
  deriving (Show, Ord, Eq)

newtype MetaLevel = MetaLevel Int

type family HasForeignLanguage (ph :: Phase) w where
  HasForeignLanguage BruijnParse a = a
  HasForeignLanguage _ a = ()

type family HasMetaLevel (ph :: Phase) w where
  HasMetaLevel _ a = a

type family HasSrcSpan (ph :: Phase) w where
  HasSrcSpan _ a = a

type family HasSrcPos (ph :: Phase) w where
  HasSrcPos BruijnParse a = a
  HasSrcPos _ a = ()

data Context (ph :: Phase) = Context
  { _lang :: HasForeignLanguage ph ForeignLanguage
  , _metaLevel :: HasMetaLevel ph MetaLevel
  , _srcSpan :: HasSrcSpan ph SrcSpan
  , _srcPos :: HasSrcPos ph SourcePos
  }
  deriving (Generic)

instance PhaseChange BruijnParse BruijnPreprocess
instance PhaseChange BruijnPreprocess BruijnToLambdaTransform
instance PhaseChange BruijnToLambdaTransform LambdaReduce

-- below is just boilerplate stuff! --
-- (insert Simpson's back fat meme) --

-- ///

class ConvertibleField src target where
  convertField :: src -> target

-- Rule 1: If the types are the same, the conversion is just 'id'.
instance {-# OVERLAPPABLE #-} (src ~ target) => ConvertibleField src target where
  convertField = id

-- Rule 2: If the target type is '()', we can always succeed by providing '()'.
instance {-# OVERLAPPING #-} ConvertibleField src () where
  convertField _ = ()

-- ///

class GAutoConvert from to where
  gautoConvert :: from x -> to x

instance (GAutoConvert f g) => GAutoConvert (M1 i c f) (M1 i c g) where
  gautoConvert (M1 x) = M1 (gautoConvert x)

instance (GAutoConvert f f', GAutoConvert g g') => GAutoConvert (f :*: g) (f' :*: g') where
  gautoConvert (a :*: b) = gautoConvert a :*: gautoConvert b

instance (ConvertibleField src target) => GAutoConvert (K1 i src) (K1 i target) where
  gautoConvert (K1 x) = K1 (convertField x)

-- ///

class PhaseChange (from :: Phase) (to :: Phase) where
  phaseChange :: Context from -> Context to
  default phaseChange ::
    (GAutoConvert (Rep (Context from)) (Rep (Context to))) =>
    Context from ->
    Context to
  phaseChange = to . gautoConvert . from
