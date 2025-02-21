-- MIT License, Copyright (c) 2025 Marvin Borner
-- simple wrapper around MonadError

module Language.Generic.Error
  ( throwError
  , runExcept
  , module Control.Monad.Except
  ) where

import           Control.Monad.Except
import           Data.Text                      ( Text )

-- type ErrorOr = MonadError Text

-- TODO: error gathering
