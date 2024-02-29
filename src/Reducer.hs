-- MIT License, Copyright (c) 2024 Marvin Borner
module Reducer
  ( reduce
  , reduceNoIO
  ) where

import           Helper
import qualified Reducer.HigherOrder           as HigherOrder
import qualified Reducer.ION                   as ION
import qualified Reducer.RKNL                  as RKNL

reduce :: EvalConf -> Expression -> IO Expression
reduce conf e = case _reducer conf of
  "RKNL"        -> RKNL.reduce e
  "ION"         -> pure $ ION.reduce e
  "HigherOrder" -> pure $ HigherOrder.reduce e
  _             -> error "Invalid reducer"

reduceNoIO :: Expression -> Expression
reduceNoIO = HigherOrder.reduce
