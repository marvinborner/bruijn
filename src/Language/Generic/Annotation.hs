-- MIT License, Copyright (c) 2025 Marvin Borner
-- inspired by Tim Williams' "Recursion Schemes"
--         and John Wiegley's hnix (BSD-3-Clause)

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Generic.Annotation
  ( AnnF
  , SrcSpan(..)
  , AnnUnit(..)
  , ann
  ) where

import           Control.Monad.State            ( MonadState
                                                , get
                                                )
import           Data.Fix                       ( Fix(..) )
import           Data.Functor.Compose           ( Compose(..) )
import           Text.Megaparsec                ( ParsecT
                                                , SourcePos
                                                , TraversableStream
                                                , getSourcePos
                                                )
import           Text.Show.Deriving             ( deriveShow1 )

data SrcSpan = SrcSpan
  { spanBegin :: SourcePos
  , spanEnd   :: SourcePos
  }
  deriving (Show, Ord, Eq)

data AnnUnit ann expr = AnnUnit
  { annotation :: ann
  , annotated  :: expr
  }
  deriving Show

deriveShow1 ''AnnUnit

type AnnF ann f = Compose (AnnUnit ann) f
type Ann ann f = Fix (AnnF ann f)

annUnitToAnn :: AnnUnit ann (f (Ann ann f)) -> Ann ann f
annUnitToAnn (AnnUnit ann a) = Fix (Compose (AnnUnit ann a))

ann1
  :: (TraversableStream s, Ord e, MonadState SourcePos m)
  => ParsecT e s m a
  -> ParsecT e s m (AnnUnit SrcSpan a)
ann1 p = do
  begin <- getSourcePos
  res   <- p
  end   <- get -- getSourcePos is expensive, therefore we store in State
  pure $ AnnUnit (SrcSpan begin end) res

ann
  :: (TraversableStream s, Ord e, MonadState SourcePos m)
  => ParsecT e s m (f (Ann SrcSpan f))
  -> ParsecT e s m (Ann SrcSpan f)
ann = (annUnitToAnn <$>) . ann1
