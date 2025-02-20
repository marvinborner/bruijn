-- MIT License, Copyright (c) 2025 Marvin Borner

module Language.Generic.Error
  ( ErrorOr
  , throwError
  , runError
  ) where

import qualified Control.Monad.Except          as Except
import           Data.Text                      ( Text )

type ErrorOr = Except.Except Text

throwError :: Text -> ErrorOr a
throwError = Except.throwError

runError = Except.runExcept
