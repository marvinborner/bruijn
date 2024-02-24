-- MIT License, Copyright (c) 2024 Marvin Borner
-- Based on Benn Lynn's ION machine and John Tromp's nf.c
-- TODO: clean everything up after tests are working
--   iter -> fold/etc, application infix vm abstraction,
--   monadic VM, map -> array?
-- (fairly literate translation from C code)
module Reducer.ION
  ( reduce
  ) where

import           Data.Bits                      ( (.|.) )
import           Data.Char                      ( chr
                                                , ord
                                                )
import           Data.List                      ( intercalate )
import qualified Data.Map                      as M
import           Data.Map                       ( Map )
import           Debug.Trace
import           Helper
import           System.IO

ncomb :: Int
ncomb = 128

spTop :: Int
spTop = maxBound

data VM = VM
  { sp   :: Int
  , hp   :: Int
  , nvar :: Int
  , mem  :: Map Int Int
  }
  deriving Show

dumpMem :: VM -> String
dumpMem VM { mem } =
  "--- mem ---\n"
    ++ intercalate
         "\n"
         (   (\(a, b) -> show a ++ ": " ++ show b)
         <$> filter (\(a, b) -> a <= 255 && b /= 0) (M.toList mem)
         )
    ++ "\n--- /mem ---"

dumpStack :: VM -> String
dumpStack VM { mem, sp } =
  "--- stack ---\n"
    ++ intercalate
         "\n"
         (   (\(a, b) ->
               show (a - sp) ++ ": " ++ show b ++ if a == sp then " <-" else ""
             )
         <$> filter (\(a, b) -> a > 255 && b /= 0) (M.toList mem)
         )
    ++ "\n--- /stack ---"

isComb :: Int -> Bool
isComb n = n < ncomb

new :: VM
new = VM spTop ncomb 0 mempty

load :: Int -> VM -> Int
load k vm = M.findWithDefault 0 k (mem vm)

store :: Int -> Int -> VM -> VM
store k v vm = vm { mem = M.insert k v $ mem vm }

app :: Int -> Int -> VM -> (Int, VM)
app x y vm@VM { hp } = (hp, store hp x $ store (hp + 1) y $ vm { hp = hp + 2 })

-- push :: Int -> VM -> VM
-- push n vm@VM { sp } = store (sp - 1) n $ vm { sp = sp - 1 }

arg' :: Int -> VM -> Int
arg' n vm = load (load (sp vm + n) vm + 1) vm

arg :: Int -> VM -> (Int, VM)
arg n vm = (arg' n vm, vm)

-- app' :: (VM -> (Int, VM)) -> (VM -> (Int, VM)) -> VM -> (Int, VM)
-- app' f g vm =
--   let (x, vm1) = f vm
--       (y, vm2) = g vm1
--   in  app x y vm2

apparg :: Int -> Int -> VM -> (Int, VM)
apparg m n vm = app (arg' m vm) (arg' n vm) vm

wor :: a -> b -> (a, b)
wor = (,)

com :: Char -> b -> (Int, b)
com = wor . ord

lazy :: Int -> (VM -> (Int, VM)) -> (VM -> (Int, VM)) -> VM -> VM
lazy d f a vm = do
  let fix i _ | i > d = Nothing
      fix i vm' =
        if load (load (sp vm' + i + 1) vm') vm' == load (sp vm' + i) vm'
          then fix (i + 1) vm'
          else do
            let vm1'       = vm' { sp = sp vm' - 2 }
            let cnvar      = nvar vm1'
            let (n1, vm2') = app (ord 'V') cnvar vm1' { nvar = cnvar + 1 }
            let spi2       = load (sp vm2' + i + 2) vm2'
            let vm3'       = store (sp vm2' + i) spi2 vm2'
            let (n2, vm4') = app spi2 n1 vm3'
            let vm5'       = store (sp vm4' + i + 1) n2 vm4'
            let (n3, vm6') = app (ord 'L') n2 vm5'
            let vm7'       = store (sp vm6' + i + 2) n3 vm6'
            let vm8' = store (load (sp vm7' + i + 3) vm7' + 1) n3 vm7'
            Just $ vm8' { sp = sp vm8' + i }
  case fix 1 vm of
    Just vm' -> vm'
    Nothing  -> do
      let (f', vm1) = f vm
      let (a', vm2) = a vm1
      let vm3       = vm2 { sp = sp vm + d }
      let
        dst = trace ("LAZY: " ++ show f' ++ " " ++ show a')
          $ load (sp vm + d + 1) vm
      store (sp vm3) f' (store dst f' (store (dst + 1) a' vm3))

-- numberArg :: Int -> VM -> Int
-- numberArg n vm = load (fst (arg n vm) + 1) vm

rules :: Int -> VM -> VM
-- rules ch vm = case chr ch of
rules ch vm = case trace (show $ chr ch) (chr ch) of
  'M' -> lvm 0 (arg 1) (arg 1)
  'Y' -> lvm 0 (arg 1) (app (ord 'Y') 1)
  'I' ->
    if trace ("I: " ++ show (arg' 2 vm) ++ " " ++ show (load (sp vm + 1) vm))
         $  arg' 2 vm
         == load (sp vm + 1) vm
      then lvm 1 (arg 1) (arg 1)
      else lvm 1 (arg 1) (arg 2)
  'F' -> do
    let xv = arg' 2 vm
    if isComb xv
      then lvm 1 (com 'I') (arg 2)
      else lvm 1 (wor $ load xv vm) (wor $ load (xv + 1) vm)
  'f' -> do
    let x = load (load (sp vm + 2) vm + 1) vm
    let (v, vm') = if isComb x
          then do
            let cnvar = nvar vm
            let (a, vm1) =
                  app (ord 'V') cnvar vm { sp = sp vm + 1, nvar = cnvar + 1 }
            let (b, vm2) = app x a vm1
            let (c, vm3) = app (ord 'L') b vm2
            (c, store (load (sp vm3 + 1) vm3 + 1) c vm3)
          else (x, vm { sp = sp vm + 1 })
    store (sp vm') v vm'
  'K' -> do
    let (xv, _) = arg 1 vm
    if isComb xv
      then lvm 1 (com 'I') (wor xv)
      else lvm 1 (wor $ load xv vm) (wor $ load (xv + 1) vm)
  'T' -> lvm 1 (arg 2) (arg 1)
  'D' -> lvm 2 (arg 1) (arg 2)
  'B' -> lvm 2 (arg 1) (apparg 2 3)
  'C' -> lvm 2 (apparg 1 3) (arg 2)
  'R' -> lvm 2 (apparg 2 3) (arg 1)
  ':' -> lvm 2 (apparg 3 1) (arg 2)
  'S' -> lvm 2 (apparg 1 3) (apparg 2 3)
  'L' -> store (sp vm) (arg' 1 vm) vm
  'V' -> do
    let iter vm'@VM { sp } | sp == spTop = vm' { sp = -1 }
        iter vm'@VM { sp } | load (load (sp + 1) vm') vm' == load sp vm' = vm'
        iter vm'@VM { sp } =
          let parent = load (sp + 1) vm'
              left   = load parent vm'
              vm''   = if left == ord 'L'
                then vm' { nvar = nvar vm' - 1 }
                else store parent (load (left + 1) vm') vm'
          in  iter vm'' { sp = sp + 1 }
    let vm1 = iter vm { sp = sp vm + 1 }
    if sp vm1 == -1
      then vm1
      else do
        let parentVal = load (load (sp vm1 + 1) vm1 + 1) vm1
        let (a, vm2)  = app (ord 'f') (load (sp vm1) vm1) vm1
        lazy 0 (wor a) (wor parentVal) (store (sp vm2) parentVal vm2)
  _ -> error "invalid combinator"
  where lvm n f g = lazy n f g vm
  -- num n = numberArg n vm

eval :: VM -> VM
eval vm@VM { sp } | sp == -1 = vm { sp = spTop }
eval vm@VM { sp } =
  -- let x1  = trace ("x1: " ++ show (load sp vm)) $ load sp vm
  let x1 =
        trace ("x1: " ++ show (load sp vm) ++ ", sp: " ++ show (spTop - sp))
          $ load sp vm
      x2  = load x1 vm
      vm1 = store (sp - 1) x2 vm { sp = sp - 1 }
      x3  = load x2 vm1
      vm2 = store (sp - 2) x3 vm1 { sp = sp - 2 }
  in  if isComb x1
        then trace ("1" ++ dumpStack vm ++ "\n" ++ dumpMem vm)
                   (eval $ rules x1 vm)
        else if isComb x2
          then trace ("2" ++ dumpStack vm1 ++ "\n" ++ dumpMem vm1)
                     (eval $ rules x2 vm1)
          else if isComb x3
            then trace ("3" ++ dumpStack vm2 ++ "\n" ++ dumpMem vm2)
                       (eval $ rules x3 vm2)
            else trace ("continue") (eval vm2)
            -- else eval $ store (sp - 1) (load n vm) vm { sp = sp - 1 }

run :: Int -> VM -> VM
run i vm@VM { sp } = eval $ store sp i vm

hasVar0 :: Int -> Int -> VM -> Bool
hasVar0 db depth vm = do
  let f = load db vm
  let a = load (db + 1) vm
  case chr f of
    'V' -> a == depth
    'L' -> hasVar0 a (depth + 1) vm
    _   -> hasVar0 f depth vm || hasVar0 a depth vm

eta :: Int -> VM -> (Int, VM)
eta x vm = do
  let f = load x vm
  let a = load (x + 1) vm
  if not (isComb f) && load a vm == ord 'V' && load (a + 1) vm == 0 && not
       (hasVar0 f 0 vm)
    then (f, vm)
    else app (ord 'L') x vm

dbIndex :: Int -> Int -> VM -> (Int, VM)
dbIndex x depth vm = do
  let f = load x vm
  let a = load (x + 1) vm
  case
      trace (show x ++ "," ++ show depth ++ ":" ++ show f ++ " " ++ show a)
            (chr f)
    of
      'V' -> app f (depth - 1 - a) vm
      'L' -> do
        let (idx, vm1) = dbIndex a (depth + 1) vm
        eta idx vm1
      _ -> do
        let (f', vm1) = dbIndex f depth vm
        let (a', vm2) = dbIndex a depth vm1
        app f' a' vm2

clapp :: (Int, Int) -> VM -> (Int, VM)
clapp (f, a) vm = case (chr f, chr a) of
  ('K', 'I')                      -> vord 'F'
  ('B', 'K')                      -> vord 'D'
  ('C', 'I')                      -> vord 'T'
  ('D', 'I')                      -> vord 'K'
  (_, 'I') | load f vm == ord 'B' -> (load (f + 1) vm, vm)
  (_, 'I') | load f vm == ord 'R' -> app (ord 'T') (load (f + 1) vm) vm
  (_, 'T') | load f vm == ord 'B' && load (f + 1) vm == ord 'C' -> vord ':'
  (_, 'I') | load f vm == ord 'S' && load (f + 1) vm == ord 'I' -> vord 'M'
  _                               -> app f a vm
  where vord c = (ord c, vm)

unDoubleVar :: Int -> Int -> VM -> (Int, VM)
unDoubleVar n db vm = do
  let f  = load db vm
  let a  = load (db + 1) vm
  let qf = load f vm == ord 'V' && load (f + 1) vm == n
  let qa = load a vm == ord 'V' && load (a + 1) vm == n
  if f == ord 'V'
    then (db, vm)
    else if f == ord 'L'
      then
        let (uda, vm') = unDoubleVar (n + 1) a vm
        in  if uda == 0 then (0, vm') else app (ord 'L') uda vm'
      else if qf && qa
        then app (ord 'V') n vm
        else if qf || qa
          then (0, vm)
          else
            let (udf, vm' ) = unDoubleVar n f vm
                (uda, vm'') = unDoubleVar n a vm'
            in  if udf == 0
                  then (0, vm')
                  else if uda == 0 then (0, vm'') else app udf uda vm''

recursive :: Int -> Int -> VM -> (Int, VM)
recursive f a vm = do
  let f'         = load (a + 1) vm
  let (f'', vm') = unDoubleVar 0 f' vm
  -- logical subtleties!
  if f == ord 'M' && load a vm == ord 'L' && load f' vm /= ord 'V'
    then if f'' /= 0 then app (ord 'L') f'' vm' else (0, vm')
    else (0, vm)

combineK :: (Int, Int, Int, Int) -> VM -> (Int, VM)
combineK huh@(n1, d1, n2, d2) vm = do
  if trace (dumpMem vm ++ "\n" ++ show huh) $ n1 == 0
    then if n2 == 0
      then clapp (d1, d2) vm
      else if load (n2 + 1) vm /= 0
        then
          let (n, vm1) = clapp (ord 'B', d1) vm
          in  combineK (0, n, load n2 vm1, d2) vm1
        else combineK (0, d1, load n2 vm, d2) vm
    else if load (n1 + 1) vm /= 0
      then if n2 == 0
        then
          let (n, vm1) = clapp (ord 'R', d2) vm
          in  combineK (0, n, load n1 vm1, d1) vm1
        else if load (n2 + 1) vm /= 0
          then
            let (n, vm1) = combineK (0, ord 'S', load n1 vm, d1) vm
            in  combineK (load n1 vm, n, load n2 vm1, d2) vm1
          else
            let (n, vm1) = combineK (0, ord 'C', load n1 vm, d1) vm
            in  combineK (load n1 vm, n, load n2 vm1, d2) vm1
      else if n2 == 0
        then combineK (load n1 vm, d1, 0, d2) vm
        else if load (n2 + 1) vm /= 0
          then if load n2 vm == 0 && load (n2 + 1) vm /= 0 && d2 == ord 'I'
            then (d1, vm) -- eta optimization
            else
              let (n, vm1) = combineK (0, ord 'B', load n1 vm, d1) vm
              in  combineK (load n1 vm, n, load n2 vm1, d2) vm1
          else combineK (load n1 vm, d1, load n2 vm, d2) vm

zipN :: Int -> Int -> VM -> (Int, VM)
zipN nf na vm | nf == 0 = (na, vm)
zipN nf na vm | na == 0 = (nf, vm)
zipN nf na vm           = do
  let (z, vm1) = zipN (load nf vm) (load na vm) vm
  app z ((load (nf + 1) vm1) .|. (load (na + 1) vm1)) vm1

convertK :: Int -> VM -> (Int, Int, VM)
convertK db vm = do
  let f = load db vm
  let a = load (db + 1) vm
  case chr f of
    'V' -> do
      let iter n 0 vm' = (n, vm')
          iter n i vm' = let (n', vm1) = app n 0 vm' in iter n' (i - 1) vm1
      let (nf, vm1) = let (n, vm') = app 0 1 vm in iter n a vm'
      (nf, ord 'I', vm1)
    'L' -> do
      let (na, ca, vm1) = convertK a vm
      let pn            = load na vm1
      if na == 0
        then let (n, vm2) = clapp (ord 'K', ca) vm1 in (0, n, vm2)
        else if load (na + 1) vm1 == 0
          then let (n, vm2) = combineK (0, ord 'K', pn, ca) vm1 in (pn, n, vm2)
          else (pn, ca, vm1)
    _ -> do
      let (nf, cf, vm1) = convertK f vm
      let (cf', a', vm2) =
            let (ca', vm') = recursive cf a vm1
            in  if nf == 0
                  then if ca' /= 0 then (ord 'Y', ca', vm') else (cf, a, vm')
                  else (cf, a, vm1)
      let (na, ca, vm3) = convertK a' vm2
      let (pn, vm4)     = zipN nf na vm3
      let (n, vm5)      = combineK (nf, cf', na, ca) vm4
      (pn, n, vm5)

toCLK :: Int -> VM -> (Int, VM)
toCLK db vm = do
  let (n, cl, vm1) = convertK db vm
  if n == 0 then (cl, vm1) else error "term not closed"

resolveExpression :: Int -> VM -> Expression
resolveExpression n _ | isComb n = error "unexpected combinator"
resolveExpression n vm           = do
  let f = load n vm
  let a = load (n + 1) vm
  case chr f of
    'V' -> Idx a
    'L' -> Abs $ resolveExpression a vm
    _   -> App (resolveExpression f vm) (resolveExpression a vm)

parseExpression :: Expression -> VM -> IO (Int, VM)
parseExpression (Abs t) vm = do
  (t', vm1) <- parseExpression t vm
  pure $ app (ord 'L') t' vm1
parseExpression (App l r) vm = do
  (l', vm1) <- parseExpression l vm
  (r', vm2) <- parseExpression r vm1
  pure $ app l' r' vm2
parseExpression (Idx i) vm = do
  pure $ app (ord 'V') i vm

go :: String -> IO ()
go s = do
  let vm   = new
  let term = parseBLC s
  (db, vm1) <- parseExpression term vm
  let (cl, vm2) = toCLK db vm1
  let (i, vm3) = app (ord 'V') (nvar vm2) vm2 { nvar = nvar vm2 + 1 }
  let (j, vm4)  = app cl i vm3
  let (k, vm5)  = app (ord 'L') j vm4
  putStrLn $ dumpStack vm2
  let res        = run k vm5
  let (idx, fin) = dbIndex (load spTop res) 0 res
  print $ resolveExpression idx fin

reduce :: Expression -> Expression
reduce e = e
