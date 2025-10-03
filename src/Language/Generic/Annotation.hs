-- MIT License, Copyright (c) 2025 Marvin Borner
-- inspired by Tim Williams' "Recursion Schemes"
--         and John Wiegley's hnix (BSD-3-Clause)
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Generic.Annotation (
  AnnF,
  pattern Ann,
  pattern AnnF,
  SrcSpan (..),
  AnnUnit (..),
  fakeSrcSpan,
  extractContext,
  fakeAnn,
  annotate,
  Ann,
  showSourcePosURI,
  showAnnotationURI,
  showSourcePos,
  showAnnotation,
  foldAnn,
  mapAnn,
  mapAnnM,
  mapAlgebra,
  mapAlgebraM,
  mapWithAnn,
  mapWithAnnM,
) where

import Control.Monad.State (
  MonadState,
  get,
 )
import Data.Context (Context (..), SrcSpan (..))
import Data.Fix (Fix (..), foldFix)
import Data.Functor.Compose (Compose (..))
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec (
  ParsecT,
  SourcePos (..),
  TraversableStream,
  getSourcePos,
  mkPos,
  unPos,
 )
import Text.Show.Deriving (deriveShow1)

showSourcePosURI :: SourcePos -> Text
showSourcePosURI (SourcePos{sourceName = file, sourceLine = line, sourceColumn = column}) =
  T.pack file
    <> ":"
    <> (T.pack . show . unPos) line
    <> ":"
    <> (T.pack . show . unPos) column

showAnnotationURI :: SrcSpan -> Text
showAnnotationURI (SrcSpan{_spanBegin = begin}) = showSourcePosURI begin

-- TODO: this will later need IO to read/highlight the annotated file
showSourcePos :: (Monad m) => SourcePos -> m Text
showSourcePos = return . showSourcePosURI

-- TODO: this will later need IO to read/highlight the annotated file
showAnnotation :: (Monad m) => SrcSpan -> m Text
showAnnotation = return . showAnnotationURI

data AnnUnit ann expr = AnnUnit
  { annotation :: ann
  , annotated :: expr
  }
  deriving (Show, Functor, Foldable, Traversable)

deriveShow1 ''AnnUnit

type AnnF ann f = Compose (AnnUnit ann) f

pattern AnnF :: ann -> f a -> Compose (AnnUnit ann) f a
pattern AnnF ann f = Compose (AnnUnit ann f)
{-# COMPLETE AnnF #-}

type Ann ann f = Fix (AnnF ann f)

pattern Ann ::
  forall ann (f :: Type -> Type).
  ann ->
  f (Ann ann f) ->
  Ann ann f
pattern Ann ann a = Fix (AnnF ann a)
{-# COMPLETE Ann #-}

annUnitToAnn :: AnnUnit ann (f (Ann ann f)) -> Ann ann f
annUnitToAnn (AnnUnit ann a) = Fix (Compose (AnnUnit ann a))

annotate1 ::
  (TraversableStream s, Ord e, MonadState (Context ph) m) =>
  ParsecT e s m a ->
  ParsecT e s m (AnnUnit (Context ph) a)
annotate1 p = do
  begin <- getSourcePos
  res <- p
  -- getSourcePos is expensive, therefore we store in State
  context@Context{_srcSpan = SrcSpan{_spanEnd = end}} <- get
  let context' = context{_srcSpan = SrcSpan begin end}
  pure $ AnnUnit context' res

annotate ::
  (TraversableStream s, Ord e, MonadState (Context ph) m) =>
  ParsecT e s m (f (Ann (Context ph) f)) ->
  ParsecT e s m (Ann (Context ph) f)
annotate = (annUnitToAnn <$>) . annotate1

fakeSrcSpan :: SrcSpan
fakeSrcSpan = SrcSpan{_spanBegin = fakeSourcePos, _spanEnd = fakeSourcePos}
 where
  fakeSourcePos =
    SourcePos
      { sourceName = "<fake>"
      , sourceLine = mkPos 1
      , sourceColumn = mkPos 1
      }

fakeAnn = Fix . AnnF fakeSrcSpan

-- utility functions --

extractContext (Ann a _) = a

foldAnn f = foldFix $ \case
  AnnF a t -> Fix $ AnnF (f a) t

mapAnn f (Fix inn) = Fix $ f inn

mapAnnM f (Fix inn) = Fix <$> f inn

mapAlgebra f = mapAnn $ \case
  AnnF a inn -> AnnF a (f inn)

mapAlgebraM f (Ann a inn) = Fix . AnnF a <$> f inn

mapWithAnn f (Ann a inn) = Fix . AnnF a . f a inn

mapWithAnnM f (Ann a inn) = Fix . AnnF a <$> f a inn

-- mapAnn f (Fix (AnnF a inn)) = Fix (AnnF (f a) inn)
