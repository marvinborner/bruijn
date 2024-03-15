-- MIT License, Copyright (c) 2024 Marvin Borner
-- TODO: This currently only really helps in edge-cases (see benchmarks) -> it's disabled by default
module Optimizer
  ( optimizedReduce
  ) where

import           Data.List                      ( tails )
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
  go p m e@(Bruijn _) = M.insertWith (++) e [p] m
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
isGood = go (10 :: Int)
 where
  go 0 _                               = True
  go i (Application (Abstraction l) r) = go (i - 2) l || go (i - 2) r
  go i (Application l               r) = go (i - 1) l || go (i - 1) r
  go i (Abstraction t                ) = go (i - 1) t
  go _ _                               = False

firstLast :: [a] -> [a]
firstLast xs@(_ : _) = tail (init xs)
firstLast _          = []

-- if expression has a parent that is also "good"
preferParent :: Expression -> [Path] -> Tree -> Bool
preferParent e ps t = any
  (\p -> any
    (\p' -> not (null p') && case resolvePath e (reverse p') of
        -- Just r  -> length (M.findWithDefault [] r t) >= length ps
      Just r  -> M.member r t
      Nothing -> False
    )
    (firstLast $ tails p)
  )
  ps

commonPath :: Path -> Path -> Path
commonPath p1 p2 = go (reverse p1) (reverse p2)
 where
  go _  [] = []
  go [] _  = []
  go (x : xs) (y : ys) | x == y    = x : go xs ys
                       | otherwise = []

incv :: Expression -> Expression
incv = go 0
 where
  go i (Bruijn j       ) = Bruijn $ if i <= j then j + 1 else j
  go i (Application a b) = Application (go i a) (go i b)
  go i (Abstraction t  ) = Abstraction $ go (i + 1) t
  go _ _                 = invalidProgramState

-- term to index
subst :: Expression -> Expression -> Expression
subst = go 0
 where
  go i n h = if n == h
    then Bruijn i
    else case h of
      (Application a b) -> Application (go i n a) (go i n b)
      (Abstraction t  ) -> Abstraction $ go (i + 1) n t
      t                 -> t

inject :: Expression -> Path -> Expression -> Expression
inject i [] e = Application (Abstraction (subst i (incv e))) i
inject i (L : p) (Application l r) = Application (inject i p l) r
inject i (R : p) (Application l r) = Application l (inject i p r)
inject i (D : p) (Abstraction t  ) = Abstraction (inject i p t)

-- TODO: Could this produce wrong (or redundant) results?
-- Optimally, `common` below would be topologically sorted so this can't happen
inject _ _       e                 = e

-- TODO: Also abstract open terms (using max index within term)
optimize :: Expression -> IO Expression
optimize e = do
  let tree = constructTree e
  let filtered =
        M.filterWithKey (\k ps -> isClosed k && isGood k && length ps > 1) tree
  -- TODO: simulated annealing on every closed term even if not good or ==1
  let filtered' =
        M.filterWithKey (\_ ps -> not $ preferParent e ps filtered) filtered
  let common = (\(k, p) -> (k, foldl1 commonPath p)) <$> M.toList filtered'
  pure $ foldl (\e' (t, p) -> inject t p e') e common

optimizedReduce :: EvalConf -> Expression -> IO Expression
optimizedReduce conf@EvalConf { _optimize = False } e = reduce conf e
optimizedReduce conf@EvalConf { _hasArg = True } (Application e arg) = do
  e' <- optimize e
  reduce conf (Application e' arg)
optimizedReduce conf e = optimize e >>= reduce conf
