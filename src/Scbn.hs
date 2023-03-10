-- MIT License, Copyright (c) 2023 Marvin Borner
-- SCBN by Balabonski, Lanco, Melquinond
-- implementation inspired by Lanco's
module Scbn
  ( reduce
  ) where

import           Data.List                      ( elemIndex )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Helper

data NameGen = NameGen Int

data STerm = SVar Int             -- x
          | SAbs Int STerm        -- λx.t
          | SApp STerm STerm      -- t1 t2
          | ES STerm Int STerm    -- t[x\u]
          deriving (Ord, Eq)

data Context = CAbs Int         -- λx.[]
             | CAppG STerm      -- [] t2
             | CAppD STerm      -- t1 []
             | CESG Int STerm   -- [] [x\t]
             | CESD STerm Int   -- t[x\[]]
             | CHole            -- []
             deriving (Ord, Eq, Show)

data Tree = Node STerm [Tree] | Leaf
  deriving Show

instance Show STerm where
  showsPrec _ (SVar x) = showString "\ESC[91m" . shows x . showString "\ESC[0m"
  showsPrec _ (SAbs x e) =
    showString "\ESC[36m["
      . shows x
      . showString ": \ESC[0m"
      . showsPrec 0 e
      . showString "\ESC[36m]\ESC[0m"
  showsPrec _ (SApp exp1 exp2) =
    showString "\ESC[33m(\ESC[0m"
      . showsPrec 0 exp1
      . showString " "
      . showsPrec 0 exp2
      . showString "\ESC[33m)\ESC[0m"
  showsPrec _ (ES exp1 x exp2) =
    showsPrec 0 exp1
      . showString "\ESC[91m[\ESC[0m"
      . showsPrec 0 x
      . showString "\\"
      . showsPrec 0 exp2
      . showString "\ESC[91m]\ESC[0m"

nextName :: NameGen -> (Int, NameGen)
nextName (NameGen x) = (x, NameGen $ x + 1)

toRedex :: Expression -> STerm
toRedex = convertWorker (NameGen 1) []
 where
  convertWorker g ns (Abstraction e) =
    let (v, g') = nextName g
        t       = convertWorker g' (v : ns) e
    in  SAbs v t
  convertWorker g ns (Application l r) =
    let lhs = convertWorker g ns l
        rhs = convertWorker g ns r
    in  SApp lhs rhs
  convertWorker _ ns (Bruijn i) =
    SVar (if i < 0 || i >= length ns then i else ns !! i)
  convertWorker _ _ _ = invalidProgramState

fromRedex :: STerm -> Expression
fromRedex = convertWorker []
 where
  convertWorker es (SAbs n e) = Abstraction $ convertWorker (n : es) e
  convertWorker es (SApp l r) =
    let lhs = convertWorker es l
        rhs = convertWorker es r
    in  Application lhs rhs
  convertWorker es (SVar n) = Bruijn $ maybe n id (elemIndex n es)
  convertWorker _  _        = invalidProgramState

isStruct :: STerm -> Maybe Int
isStruct (SAbs _  _ ) = Nothing
isStruct (SApp t1 _ ) = isStruct t1
isStruct (SVar x    ) = Just x
isStruct (ES t1 x t2) = case isStruct t1 of
  a@(Just x') | x' == x   -> isStruct t2
              | otherwise -> a
  a -> a

isNF :: Bool -> STerm -> Bool
isNF at t =
  let cont     = context at t
      listEval = Set.fold
        (\(i, c, at') acc -> (map (\z -> assemble z c) (eval at' i)) ++ acc)
        []
        cont
  in  not $ any (\i -> i /= t) listEval

findLambda :: (STerm -> t) -> STerm -> Maybe (STerm -> t, STerm)
findLambda c (SAbs v t  ) = Just (c, SAbs v t)
findLambda c (ES t1 x t2) = findLambda (\a -> c (ES a x t2)) t1
findLambda _ _            = Nothing

context :: Bool -> STerm -> Set (STerm, [Scbn.Context], Bool)
context inert w = Set.map (\(a, b, at) -> (a, reverse b, at))
                          (aux Set.empty inert [CHole] w)
 where
  aux phi at c term = case term of
    SAbs x t -> aux (if at then phi else Set.insert x phi) at ((CAbs x) : c) t
    SApp t1 t2 ->
      let r = Set.insert (SApp t1 t2, c, at) (aux phi True ((CAppG t2) : c) t1)
      in  case isStruct t1 of
            Just x
              | Set.member x phi -> Set.union
                r
                (aux phi False ((CAppD t1) : c) t2)
              | otherwise -> r
            _ -> r
    SVar x -> Set.singleton (SVar x, c, at)
    ES t1 x t2 ->
      let newT1 = aux phi at ((CESG x t2) : c) t1
          c1    = if any (\(y, _, _) -> y == (SVar x)) newT1
            then aux phi True ((CESD t1 x) : c) t2
            else Set.empty
          c2 = case isStruct t2 of
            Just x'
              | Set.member x' phi -> aux (Set.insert x phi)
                                         at
                                         ((CESG x t2) : c)
                                         t1
              | otherwise -> newT1
            _ -> newT1
      in  Set.insert (ES t1 x t2, c, at) (Set.union c1 c2)

eval :: Bool -> STerm -> [STerm]
eval _ (SApp t1 t2) =
  let r = case findLambda (\i -> i) t1 of
        Just (f, SAbs x t) -> f (ES t x t2)
        _                  -> SApp t1 t2
  in  [r]
eval _ (ES t1 x t2) =
  let isVal = findLambda (\i -> i) t2
  in  case isVal of
        Just (cont, term)
          | isNF True term
          -> let cT1           = context False t1
                 contextFilter = Set.filter (\(i, _, _) -> i == SVar x) cT1
             in  if Set.null contextFilter
                   then [ES t1 x t2]
                   else map (\(_, c, _) -> cont (ES (assemble term c) x term))
                            (Set.elems contextFilter)
          | otherwise
          -> [ES t1 x t2]
        _ -> [ES t1 x t2]
eval _ x = [x]

assemble :: STerm -> [Scbn.Context] -> STerm
assemble = foldr
  (\i acc -> case i of
    CAbs  v    -> SAbs v acc
    CAppG t'   -> SApp acc t'
    CAppD t'   -> SApp t' acc
    CESG v  t' -> ES acc v t'
    CESD t' v  -> ES t' v acc
    CHole      -> acc
  )

allEval :: STerm -> [STerm]
allEval t =
  let cont     = context False t
      listEval = Set.fold
        (\(i, c, at) acc -> (map (\z -> assemble z c) (eval at i)) ++ acc)
        []
        cont
  in  filter (\i -> i /= t) listEval

clear :: NameGen -> Set Int -> STerm -> (STerm, Set Int, NameGen)
clear g env (SAbs x t) =
  let (newX, g')      = if Set.member x env then nextName g else (x, g)
      (t', env', g'') = clear g' (Set.insert newX env) (replace x newX t)
  in  (SAbs newX t', env', g'')
clear g env (SApp t u) =
  let (t' , env' , g' ) = clear g env t
      (t'', env'', g'') = clear g' env' u
  in  (SApp t' t'', env'', g'')
clear g env (SVar x) = (SVar x, env, g)
clear g env (ES t x u) =
  let (t' , env' , g' ) = clear g env t
      (t'', env'', g'') = clear g' env' u
  in  (ES t' x t'', env'', g'')

replace :: Int -> Int -> STerm -> STerm
replace o n (SAbs x t) =
  let newX = if x == o then n else x in SAbs newX (replace o n t)
replace o n (SApp t u) = SApp (replace o n t) (replace o n u)
replace o n (SVar x  ) = SVar (if x == o then n else x)
replace o n (ES t x u) =
  let newX = if x == o then n else x in ES (replace o n t) newX (replace o n u)

subst :: STerm -> Int -> STerm -> STerm
subst t@(SVar v  ) x s = if v == x then s else t
subst (  SAbs n t) x s = SAbs n (subst t x s)
subst (  SApp l r) x s = SApp (subst l x s) (subst r x s)
subst t            _ _ = t

star :: STerm -> STerm
star (SAbs n t) = SAbs n (star t)
star (SApp l r) = SApp (star l) (star r)
star (ES t x s) = star $ subst (star t) x s
star t          = t

reduce :: Expression -> Expression
reduce term =
  let go t gen =
        let (t', _, gen') = clear gen Set.empty t
            mapgen (k : ts) genm =
              let (g, genm') = go k genm in g : (mapgen ts genm')
            mapgen [] _ = []
        in  (t' : (concat $ mapgen (allEval t') gen'), gen')
      (red, _) = go (toRedex term) (NameGen 100000)
  in  fromRedex $ star $ last red
