-- MIT License, Copyright (c) 2025 Marvin Borner
-- inspired by Tim Williams' "Recursion Schemes"
--         and John Wiegley's hnix (BSD-3-Clause)

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Generic.Annotation
  ( AnnF
  , pattern AnnF
  , SrcSpan(..)
  , AnnUnit(..)
  , ann
  , showAnnotation
  ) where

import           Control.Monad.State            ( MonadState
                                                , get
                                                )
import           Data.Fix                       ( Fix(..) )
import           Data.Functor.Compose           ( Compose(..) )
import           Data.Kind                      ( Type )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Text.Megaparsec                ( ParsecT
                                                , SourcePos(..)
                                                , unPos
                                                , TraversableStream
                                                , getSourcePos
                                                )
import           Text.Show.Deriving             ( deriveShow1 )

data SrcSpan = SrcSpan
  { spanBegin :: SourcePos
  , spanEnd   :: SourcePos
  }
  deriving (Show, Ord, Eq)

-- TODO: this will later need IO to read/highlight the annotated file
showSourcePos :: (Monad m) => SourcePos -> m Text
showSourcePos (SourcePos { sourceName = file, sourceLine = line, sourceColumn = column }) =
  return $ T.pack file <> ":" <> (T.pack . show . unPos) line <> ":" <> (T.pack . show . unPos) column

showAnnotation :: (Monad m) => SrcSpan -> m Text
showAnnotation (SrcSpan { spanBegin = begin, spanEnd = end }) = do
  begin' <- showSourcePos begin
  end' <- showSourcePos end
  return $ begin' <> " to " <> end'

data AnnUnit ann expr = AnnUnit
  { annotation :: ann
  , annotated  :: expr
  }
  deriving (Show, Functor, Foldable, Traversable)

deriveShow1 ''AnnUnit

type AnnF ann f = Compose (AnnUnit ann) f

pattern AnnF :: ann -> f a -> Compose (AnnUnit ann) f a
pattern AnnF ann f = Compose (AnnUnit ann f)
{-# complete AnnF #-}

type Ann ann f = Fix (AnnF ann f)

pattern Ann
  :: forall ann (f :: Type -> Type) . ann
  -> f (Ann ann f)
  -> Ann ann f
pattern Ann ann a = Fix (AnnF ann a)
{-# complete Ann #-}

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
