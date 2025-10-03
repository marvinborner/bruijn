-- MIT License, Copyright (c) 2025 Marvin Borner
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, FlexibleInstances, KindSignatures, TypeApplications, ScopedTypeVariables, UndecidableInstances #-}

module Language.Generic.Error
  ( showError
  , Error(..)
  , module Control.Monad.Except
  , liftPhase
  , PhaseT(..)
  , runPhaseTOrFail
  ) where

import           Control.Monad.Except
import           Control.Monad.State            ( MonadIO
                                                , StateT
                                                )
import           Data.Context                   ( Context(..) )
import           Data.Phase                     ( HasSPhase
                                                , Phase(..)
                                                , sphase
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T

prettyError :: Text -> Text -> Text
prettyError phase msg =
  "Error while \ESC[101m\ESC[30m" <> phase <> "\ESC[0m: " <> msg

data Error ph = Error (Context ph) Text

-- TODO: this will later need IO to read/highlight the annotated file
class ShowError ph where
  showError :: Error ph -> Text

instance {-# OVERLAPPABLE #-} forall ph. HasSPhase ph => ShowError ph where
  showError (Error ctx msg) = prettyError (T.pack $ show (sphase @ph)) msg

-- instance ShowError BruijnToLambdaTransform where
--   showError (Error ctx msg) = msg

newtype PhaseT m a = PhaseT { runPhaseT :: ExceptT Text m a }
  deriving (Functor, Applicative, Monad, MonadError Text, MonadIO)

-- runPhaseTOrFail :: (MonadIO m, PhaseError m) => PhaseContext -> PhaseT m Term -> m Term
runPhaseTOrFail context p = do
  res <- runExceptT $ runPhaseT p
  case res of
    Left  err -> throwError $ Error context err
    Right ok  -> return ok

-- every phase has a different PhaseError, so we use showError!
liftPhase phase = PhaseT $ ExceptT $ do
  result <- runExceptT phase
  return $ case result of
    Left  err -> Left (showError err)
    Right x   -> Right x
