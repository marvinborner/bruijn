-- MIT License, Copyright (c) 2024 Marvin Borner
module Optimizer
  ( optimizedReduce
  ) where

import qualified Data.Map                      as M
import           Helper
import           Reducer

data Direction = L | D | R
  deriving (Show, Eq)
type Path = [Direction] -- will be reversed
type Tree = M.Map Expression [Path]

resolvePath :: Expression -> Path -> Maybe Expression
resolvePath e                 []      = Just e
resolvePath (Application l _) (L : p) = resolvePath l p
resolvePath (Application _ r) (R : p) = resolvePath r p
resolvePath (Abstraction t  ) (D : p) = resolvePath t p
resolvePath _                 _       = Nothing

constructTree :: Expression -> Tree
constructTree = go [] M.empty
 where
  go p m e@(Application l r) = do
    let m'  = go (L : p) m l
    let m'' = go (R : p) m' r
    M.insertWith (++) e [p] m''
  go p m e@(Abstraction t) = do
    let m' = go (D : p) m t
    M.insertWith (++) e [p] m'
  go p m e@(Bruijn i) = M.insertWith (++) e [p] m
  go _ _ _            = invalidProgramState

isClosed :: Expression -> Bool
isClosed = go 0
 where
  go i (Bruijn j       ) = i > j
  go i (Application l r) = go i l && go i r
  go i (Abstraction t  ) = go (i + 1) t
  go _ _                 = True

-- (kinda arbitrary)
isGood :: Expression -> Bool
isGood = go 10
 where
  go 0 _                               = True
  go i (Application (Abstraction l) r) = go (i - 2) l || go (i - 2) r
  go i (Application l               r) = go (i - 1) l || go (i - 1) r
  go i (Abstraction t                ) = go (i - 1) t
  go _ _                               = False

-- if expression has a parent that appears more often
preferParent :: Expression -> [Path] -> Tree -> Bool
preferParent e ps t = do
  let len = length ps
  any
    (\p -> any
      (\p' -> case resolvePath e p' of
        Just r  -> length (M.findWithDefault [] r t) >= len
        Nothing -> False
      )
      [R : p, D : p, L : p]
    )
    ps

commonPath :: Path -> Path -> Path
commonPath p1 p2 = go (reverse p1) (reverse p2)
 where
  go _  [] = []
  go [] _  = []
  go (x : xs) (y : ys) | x == y    = x : go xs ys
                       | otherwise = []

-- inject :: Expression -> Path -> Expression -> [Path] -> Expression
-- inject i [] e ps = Application (Abstraction (subst ? (incv e))) i
-- inject i [L : p] (Application l r) ps = Application (inject i p l ps) r
-- inject i [R : p] (Application l r) ps = Application l (inject i p r ps)
-- inject i [D : p] (Abstraction t  ) ps = Abstraction (inject i p t ps)
-- inject _ _       _                 _  = invalidProgramState

optimize :: Expression -> IO Expression
optimize e = do
  let tree = constructTree e
  let filtered =
        M.filterWithKey (\k ps -> isClosed k && isGood k && length ps > 1) tree
  -- TODO: simulated annealing on every closed term even if not good or ==1
  let filtered' =
        M.filterWithKey (\k ps -> not $ preferParent e ps filtered) filtered
  print $ (\(k, p) -> foldl1 commonPath p) <$> M.toList filtered'
  -- inject t (take (length commonPath) ps) e ps -- oder so
  pure e
-- optimize e = constructTree e

-- TODO: enable optimizer with flag
optimizedReduce :: EvalConf -> Expression -> IO Expression
optimizedReduce conf e = do
  -- optimize e
  reduce conf e
