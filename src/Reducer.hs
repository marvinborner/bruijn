module Reducer
  ( reduce
  ) where

import           Data.Bifunctor                 ( first )
import           Data.Set                hiding ( fold )
import           Parser

-- TODO: Reduce variable -> later: only reduce main in non-repl
-- TODO: Use zero-based indexing (?)

shiftUp :: Expression -> Int -> Expression
shiftUp (Bruijn x) n = if x > n then Bruijn (pred x) else Bruijn x
shiftUp (Application exp1 exp2) n =
  Application (shiftUp exp1 n) (shiftUp exp2 n)
shiftUp (Abstraction exp) n = Abstraction (shiftUp exp (succ n))

shiftDown :: Expression -> Int -> Expression
shiftDown (Bruijn x) n = if x > n then Bruijn (succ x) else Bruijn x
shiftDown (Application exp1 exp2) n =
  Application (shiftDown exp1 n) (shiftDown exp2 n)
shiftDown (Abstraction exp) n = Abstraction (shiftDown exp (succ n))

bind :: Expression -> Expression -> Int -> Expression
bind exp (Bruijn x) n = if x == n then exp else Bruijn x
bind exp (Application exp1 exp2) n =
  Application (bind exp exp1 n) (bind exp exp2 n)
bind exp (Abstraction exp') n =
  Abstraction (bind (shiftDown exp 0) exp' (succ n))

step :: Expression -> Expression
step (Bruijn exp) = Bruijn exp
step (Application (Abstraction exp) app) =
  shiftUp (bind (shiftDown app 0) exp 1) 1
step (Application exp1 exp2) = Application (step exp1) (step exp2)
step (Abstraction exp      ) = Abstraction (step exp)

reduceable :: Expression -> Bool
reduceable (Bruijn _                        ) = False
reduceable (Application (Abstraction _) _   ) = True
reduceable (Application exp1 exp2) = reduceable exp1 || reduceable exp2
reduceable (Abstraction exp                 ) = reduceable exp

-- alpha conversion is not needed with de bruijn indexing
reduce :: Expression -> Expression
reduce = until (not . reduceable) step
