-- MIT License, Copyright (c) 2025 Marvin Borner
-- TODO: UI/Debugger/RKNL which links to Bruijn/Debugger/RKNL?

module Language.Generic.Debugger where

data State = State
  { stk :: [Text]
  }

-- debug :: m Debug
