-- MIT License, Copyright (c) 2024 Marvin Borner
module Reducer
  ( reduce
  , unsafeReduce
  ) where

import           Helper
import qualified Reducer.ION                   as ION
import qualified Reducer.RKNL                  as RKNL

reduce :: EvalConf -> Expression -> IO Expression
reduce conf e = case _reducer conf of
  "RKNL" -> RKNL.reduce e
  "ION"  -> pure $ ION.reduce e
  _      -> error "Invalid reducer"

unsafeReduce :: Expression -> Expression
unsafeReduce = RKNL.unsafeReduce
