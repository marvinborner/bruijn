-- MIT License, Copyright (c) 2025 Marvin Borner
-- inspired by Tim Williams' "Recursion Schemes"
--         and John Wiegley's hnix (BSD-3-Clause)

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Generic.Annotation
  ( AnnF
  , pattern AnnF
  , pattern FixAnnF
  , SrcSpan(..)
  , AnnUnit(..)
  , fakeSrcSpan
  , fakeContext
  , fakeAnn
  , ann
  , fixAnnF
  , showSourcePosURI
  , showAnnotationURI
  , showSourcePos
  , showAnnotation
  , foldAnn
  , mapAnn
  , mapAnnM
  , mapAlgebra
  , mapAlgebraM
  , mapWithAnn
  , mapWithAnnM
  ) where

import           Control.Monad.State            ( MonadState
                                                , get
                                                )
import           Data.Fix                       ( Fix(..), foldFix )
import           Data.Functor.Compose           ( Compose(..) )
import           Data.Kind                      ( Type )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Text.Megaparsec                ( ParsecT
                                                , SourcePos(..)
                                                , unPos
                                                , TraversableStream
                                                , getSourcePos
                                                , mkPos
                                                )
import           Text.Show.Deriving             ( deriveShow1 )
import Data.Context (Context(..), SrcSpan(..))

showSourcePosURI :: SourcePos -> Text
showSourcePosURI (SourcePos { sourceName = file, sourceLine = line, sourceColumn = column })
  = T.pack file
    <> ":" <> (T.pack . show . unPos) line
    <> ":"
    <> (T.pack . show . unPos) column

showAnnotationURI :: SrcSpan -> Text
showAnnotationURI (SrcSpan { spanBegin = begin }) = showSourcePosURI begin

-- TODO: this will later need IO to read/highlight the annotated file
showSourcePos :: (Monad m) => SourcePos -> m Text
showSourcePos = return . showSourcePosURI

-- TODO: this will later need IO to read/highlight the annotated file
showAnnotation :: (Monad m) => SrcSpan -> m Text
showAnnotation = return . showAnnotationURI

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

pattern FixAnnF ann term = Fix (AnnF ann term)
fixAnnF ann term = Fix $ AnnF ann term

annUnitToAnn :: AnnUnit ann (f (Ann ann f)) -> Ann ann f
annUnitToAnn (AnnUnit ann a) = Fix (Compose (AnnUnit ann a))

ann1
  :: (TraversableStream s, Ord e, MonadState (Context ph) m)
  => ParsecT e s m a
  -> ParsecT e s m (AnnUnit (Context ph) a)
ann1 p = do
  begin <- getSourcePos
  res   <- p
  -- getSourcePos is expensive, therefore we store in State
  context@Context { srcSpan = SrcSpan { spanEnd = end } } <- get 
  let context' = context { srcSpan = SrcSpan begin end }
  pure $ AnnUnit context' res

ann
  :: (TraversableStream s, Ord e, MonadState (Context ph) m)
  => ParsecT e s m (f (Ann (Context ph) f))
  -> ParsecT e s m (Ann (Context ph) f)
ann = (annUnitToAnn <$>) . ann1

fakeSrcSpan :: SrcSpan
fakeSrcSpan = fakeSrcSpan
  where
    fakeSourcePos = SourcePos { sourceName = "<fake>", sourceLine = mkPos 1, sourceColumn = mkPos 1 }
    fakeSrcSpan = SrcSpan { spanBegin = fakeSourcePos, spanEnd = fakeSourcePos }

fakeAnn = Fix . AnnF fakeSrcSpan

-- TODO: move?
fakeContext = Fix . AnnF Context { srcSpan = fakeSrcSpan }

-- utility functions --

foldAnn f = foldFix $ \case
  AnnF a t -> Fix $ AnnF (f a) t

mapAnn f (Fix inn) = Fix $ f inn

mapAnnM f (Fix inn) = Fix <$> f inn

mapAlgebra f = mapAnn $ \case
  AnnF a inn -> AnnF a (f inn)

mapAlgebraM f (Fix (AnnF a inn)) = Fix . AnnF a <$> f inn

mapWithAnn f (Fix (AnnF a inn)) = Fix . AnnF a . f a inn

mapWithAnnM f (Fix (AnnF a inn)) = Fix . AnnF a <$> f a inn

-- mapAnn f (Fix (AnnF a inn)) = Fix (AnnF (f a) inn)
