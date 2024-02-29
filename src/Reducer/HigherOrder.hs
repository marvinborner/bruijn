-- MIT License, Copyright (c) 2024 Marvin Borner
-- Slightly modified version of Tromp's AIT/Lambda.lhs reducer implementation
module Reducer.HigherOrder
  ( reduce
  ) where

import           Helper

data HigherOrder = HigherOrderBruijn Int | HigherOrderAbstraction (HigherOrder -> HigherOrder) | HigherOrderApplication HigherOrder HigherOrder
data NamedTerm = NamedVariable Int | NamedAbstraction Int NamedTerm | NamedApplication NamedTerm NamedTerm

app :: HigherOrder -> HigherOrder -> HigherOrder
app (HigherOrderAbstraction f) = f
app f                          = HigherOrderApplication f

eval :: Expression -> HigherOrder
eval = go []
 where
  go env (Bruijn      x    ) = env !! x
  go env (Abstraction e    ) = HigherOrderAbstraction $ \x -> go (x : env) e
  go env (Application e1 e2) = app (go env e1) (go env e2)
  go _   _                   = invalidProgramState

toNamedTerm :: HigherOrder -> NamedTerm
toNamedTerm = go 0
 where
  go _ (HigherOrderBruijn i) = NamedVariable i
  go d (HigherOrderAbstraction f) =
    NamedAbstraction d $ go (d + 1) (f (HigherOrderBruijn d))
  go d (HigherOrderApplication e1 e2) = NamedApplication (go d e1) (go d e2)

resolveExpression :: NamedTerm -> Expression
resolveExpression = resolve []
 where
  resolve vs (NamedVariable i     ) = Bruijn $ vs !! i
  resolve vs (NamedAbstraction v t) = Abstraction $ resolve (v : vs) t
  resolve vs (NamedApplication l r) = Application (resolve vs l) (resolve vs r)

reduce :: Expression -> Expression
reduce = resolveExpression . toNamedTerm . eval
