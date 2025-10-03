-- MIT License, Copyright (c) 2025 Marvin Borner
-- inspired by John Wiegley's hnix (BSD-3-Clause)

module Language.Bruijn.Tracer (
  trace,
  traceAnn,
) where

import Control.Applicative (Alternative)
import Control.Monad (
  guard,
  (<=<),
 )
import Control.Monad.IO.Class (
  MonadIO,
  liftIO,
 )
import Control.Monad.Reader (
  ask,
  local,
  runReaderT,
 )
import Data.Bruijn (
  Term,
  TermAnn,
  TermAnnF,
  TermF,
 )
import Data.Fix (Fix (..))

adiM ::
  (Traversable t, Monad m) =>
  (t a -> m a) ->
  ((Fix t -> m a) -> Fix t -> m a) ->
  Fix t ->
  m a
adiM f g = g ((f <=< traverse (adiM f g)) . unFix)

trace ::
  (MonadIO m, MonadIO n, Alternative n) =>
  (TermF (m v) -> m v) ->
  Term ->
  n (m v)
trace func = flip runReaderT 0 . adiM (pure <$> func) psi
 where
  psi k v = do
    depth <- ask
    guard (depth < 200)
    local succ $ do
      action <- k v
      return $ do
        liftIO $ print v
        res <- action
        liftIO $ putStrLn "."
        return res

traceAnn ::
  (MonadIO m, MonadIO n, Alternative n) =>
  (TermAnnF (m v) -> m v) ->
  TermAnn ->
  n (m v)
traceAnn func = flip runReaderT 0 . adiM (pure <$> func) psi
 where
  psi k v = do
    depth <- ask
    guard (depth < 200)
    local succ $ do
      action <- k v
      return $ do
        liftIO $ print v
        res <- action
        liftIO $ putStrLn "."
        return res
