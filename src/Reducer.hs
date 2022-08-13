module Reducer
  ( reduce
  ) where

import           Helper

-- TODO: Research interaction nets and optimal reduction

-- TODO: Eta-reduction: [f 0] => f
--   (Abstraction f@_ (Bruijn 0)) = f

(<+>) :: Expression -> Int -> Expression
(<+>) (Bruijn x         ) n = if x > n then Bruijn (pred x) else Bruijn x
(<+>) (Application e1 e2) n = Application (e1 <+> n) (e2 <+> n)
(<+>) (Abstraction e    ) n = Abstraction $ e <+> (succ n)
(<+>) _                   _ = error "invalid"

(<->) :: Expression -> Int -> Expression
(<->) (Bruijn x         ) n = if x > n then Bruijn (succ x) else Bruijn x
(<->) (Application e1 e2) n = Application (e1 <-> n) (e2 <-> n)
(<->) (Abstraction e    ) n = Abstraction $ e <-> (succ n)
(<->) _                   _ = error "invalid"

bind :: Expression -> Expression -> Int -> Expression
bind e (Bruijn x         ) n = if x == n then e else Bruijn x
bind e (Application e1 e2) n = Application (bind e e1 n) (bind e e2 n)
bind e (Abstraction exp' ) n = Abstraction (bind (e <-> (-1)) exp' (succ n))
bind _ _                   _ = error "invalid"

step :: Expression -> Expression
step (Bruijn e) = Bruijn e
step (Application (Abstraction e) app) = (bind (app <-> (-1)) e 0) <+> 0
step (Application e1 e2) = Application (step e1) (step e2)
step (Abstraction e) = Abstraction (step e)
step _ = error "invalid"

-- until eq
converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)

-- alpha conversion is not needed with de bruijn indexing
reduce :: Expression -> Expression
reduce = converge step
