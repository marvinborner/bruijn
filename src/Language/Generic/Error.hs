-- MIT License, Copyright (c) 2025 Marvin Borner
{-# LANGUAGE FlexibleInstances #-}

module Language.Generic.Error
  ( throwError
  , runError
  , runErrorT
  , showError
  , Error(..)
  , MonadError
  ) where

import qualified Control.Monad.Except          as Except
import           Control.Monad.State            ( StateT )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Language.Generic.Annotation    ( SrcSpan
                                                , showAnnotation
                                                )

errorPrefix :: Text
errorPrefix = "\ESC[101m\ESC[30mERROR\ESC[0m "

data Error = ParseError Text | TransformError SrcSpan Text | PreprocessError SrcSpan Text

-- TODO: this will later need IO to read/highlight the annotated file
showError :: (Monad m) => Error -> m Text
showError (ParseError msg) = return $ errorPrefix <> "while parsing: " <> msg
showError (TransformError ann msg) = do
  ann' <- showAnnotation ann
  return $ errorPrefix <> "while transforming: " <> ann' <> ": " <> msg
showError (PreprocessError ann msg) = do
  ann' <- showAnnotation ann
  return $ errorPrefix <> "while preprocessing: " <> ann' <> ": " <> msg

type ErrorT m a = Except.ExceptT Error m a
type ErrorM a = Except.Except Error a

runErrorT :: ErrorT m a -> m (Either Error a)
runErrorT = Except.runExceptT

runError :: ErrorM a -> Either Error a
runError = Except.runExcept

class Monad m => MonadError m where
  throwError :: Error -> m a
  catchError :: m a -> (Error -> m a) -> m a

instance Monad m => MonadError (Except.ExceptT Error m) where
  throwError = Except.throwError
  catchError = Except.catchError
