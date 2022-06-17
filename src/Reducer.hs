module Reducer
  ( reduce
  ) where

import           Helper

-- TODO: Research interaction nets and optimal reduction

-- TODO: Eta-reduction: [f 0] => f
--   (Abstraction f@_ (Bruijn 0)) = f

(<+>) :: Expression -> Int -> Expression
(<+>) (Bruijn x             ) n = if x > n then Bruijn (pred x) else Bruijn x
(<+>) (Application exp1 exp2) n = Application (exp1 <+> n) (exp2 <+> n)
(<+>) (Abstraction exp      ) n = Abstraction $ exp <+> (succ n)

(<->) :: Expression -> Int -> Expression
(<->) (Bruijn x             ) n = if x > n then Bruijn (succ x) else Bruijn x
(<->) (Application exp1 exp2) n = Application (exp1 <-> n) (exp2 <-> n)
(<->) (Abstraction exp      ) n = Abstraction $ exp <-> (succ n)

bind :: Expression -> Expression -> Int -> Expression
bind exp (Bruijn x) n = if x == n then exp else Bruijn x
bind exp (Application exp1 exp2) n =
  Application (bind exp exp1 n) (bind exp exp2 n)
bind exp (Abstraction exp') n = Abstraction (bind (exp <-> (-1)) exp' (succ n))

step :: Expression -> Expression
step (Bruijn exp                        ) = Bruijn exp
step (Application (Abstraction exp) app ) = (bind (app <-> (-1)) exp 0) <+> 0
step (Application exp1              exp2) = Application (step exp1) (step exp2)
step (Abstraction exp                   ) = Abstraction (step exp)

reduceable :: Expression -> Bool
reduceable (Bruijn   _                      ) = False
reduceable (Variable _                      ) = True
reduceable (Application (Abstraction _) _   ) = True
reduceable (Application exp1 exp2) = reduceable exp1 || reduceable exp2
reduceable (Abstraction exp                 ) = reduceable exp

-- alpha conversion is not needed with de bruijn indexing
reduce :: Expression -> Expression
reduce = until (not . reduceable) step
