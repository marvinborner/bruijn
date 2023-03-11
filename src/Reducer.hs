-- MIT License, Copyright (c) 2023 Marvin Borner
-- based on the RKNL abstract machine
module Reducer
  ( reduce
  ) where

import           Control.Concurrent.MVar
import           Data.List                      ( elemIndex )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( fromMaybe )
import           Helper

type Store = Map Int Box
type Stack = [Redex]

newtype NameGen = NameGen Int
data BoxValue = Todo Redex | Done Redex | Empty
newtype Box = Box (MVar BoxValue)
data Rvar = Num Int | Hole
data Redex = Rabs Int Redex | Rapp Redex Redex | Rvar Rvar | Rclosure Redex Store | Rcache Box Redex
data Conf = Econf NameGen Redex Store Stack | Cconf NameGen Stack Redex | End

nextName :: NameGen -> (Int, NameGen)
nextName (NameGen x) = (x, NameGen $ x + 1)

toRedex :: Expression -> Redex
toRedex = convertWorker (NameGen 1) []
 where
  convertWorker g ns (Abstraction e) =
    let (v, g') = nextName g
        t       = convertWorker g' (v : ns) e
    in  Rabs v t
  convertWorker g ns (Application l r) =
    let lhs = convertWorker g ns l
        rhs = convertWorker g ns r
    in  Rapp lhs rhs
  convertWorker _ ns (Bruijn i) =
    Rvar $ Num (if i < 0 || i >= length ns then i else ns !! i)
  convertWorker _ _ _ = invalidProgramState

fromRedex :: Redex -> Expression
fromRedex = convertWorker []
 where
  convertWorker es (Rabs n e) = Abstraction $ convertWorker (n : es) e
  convertWorker es (Rapp l r) =
    let lhs = convertWorker es l
        rhs = convertWorker es r
    in  Application lhs rhs
  convertWorker es (Rvar (Num n)) = Bruijn $ fromMaybe n (elemIndex n es)
  convertWorker _  _              = invalidProgramState

transition :: Conf -> IO Conf
transition (Econf g (Rapp u v) e s) =
  pure $ Econf g u e (Rapp (Rvar Hole) (Rclosure v e) : s)
transition (Econf g (Rabs x t) e s) = do
  box <- newMVar Empty
  pure $ Cconf g s (Rcache (Box box) (Rclosure (Rabs x t) e))
transition (Econf g (Rvar (Num x)) e s) = do
  def <- newMVar $ Done $ Rvar $ Num x
  let b@(Box m) = Map.findWithDefault (Box def) x e
  rd <- readMVar m
  case rd of
    Todo (Rclosure v e') -> pure $ Econf g v e' (Rcache b (Rvar Hole) : s)
    Done t               -> pure $ Cconf g s t
    _                    -> invalidProgramState
transition (Cconf g ((Rcache (Box m) (Rvar Hole)) : s) t) = do
  modifyMVar_ m (\_ -> pure $ Done t)
  pure $ Cconf g s t
transition (Cconf g ((Rapp (Rvar Hole) ve) : s) (Rcache _ (Rclosure (Rabs x t) e)))
  = do
    box <- newMVar (Todo ve)
    pure $ Econf g t (Map.insert x (Box box) e) s
transition (Cconf g s (Rcache (Box m) (Rclosure (Rabs x t) e))) = do
  rd <- readMVar m
  case rd of
    Done v -> pure $ Cconf g s v
    Empty  -> do
      let (x1, g') = nextName g
      box <- newMVar $ Done $ Rvar $ Num x1
      pure $ Econf g'
                   t
                   (Map.insert x (Box box) e)
                   (Rabs x1 (Rvar Hole) : Rcache (Box m) (Rvar Hole) : s)
    Todo _ -> invalidProgramState
transition (Cconf g ((Rapp (Rvar Hole) (Rclosure v e)) : s) t) =
  pure $ Econf g v e (Rapp t (Rvar Hole) : s)
transition (Cconf g ((Rapp t (Rvar Hole)) : s) v) = pure $ Cconf g s (Rapp t v)
transition (Cconf g ((Rabs x1 (Rvar Hole)) : s) v) =
  pure $ Cconf g s (Rabs x1 v)
transition (Cconf _ [] _) = pure End
transition _              = invalidProgramState

forEachState :: Conf -> (Conf -> IO Conf) -> IO Conf
forEachState conf trans = trans conf >>= \case
  End  -> pure conf
  next -> forEachState next trans

-- TODO: NameGen is arbitrary to not conflict with toRedex
loadTerm :: Redex -> Conf
loadTerm t = Econf (NameGen 1000000) t Map.empty []

reduce :: Expression -> IO Expression
reduce e = do
  let redex = toRedex e
  forEachState (loadTerm redex) transition >>= \case
    Cconf _ [] v -> pure $ fromRedex v
    _            -> invalidProgramState
