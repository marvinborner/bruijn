-- MIT License, Copyright (c) 2023 Marvin Borner
-- based on the RKNL abstract machine
module Reducer
  ( reduce
  ) where

import           Data.IORef
import           Data.List                      ( elemIndex )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Helper
import           System.Random           hiding ( next )

type Store = Map Int Box
type Stack = [Redex]

data BoxValue = Todo Redex | Done Redex | Empty
data Box = Box (IORef BoxValue)
data Rvar = Num Int | Hole
data Redex = Rabs Int Redex | Rapp Redex Redex | Rvar Rvar | Rclosure Redex Store | Rcache Box Redex
data Conf = Econf Redex Store Stack | Cconf Stack Redex | End

toRedex :: Expression -> Redex
toRedex = convertWorker (mkStdGen 42) []
 where
  convertWorker g ns (Abstraction e) =
    let (v, g') = uniform g :: (Int, StdGen)
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
  convertWorker es (Rvar (Num n)) = Bruijn $ maybe n id (elemIndex n es)
  convertWorker _  _              = invalidProgramState

transition :: Conf -> IO Conf
transition (Econf (Rapp u v) e s) =
  pure $ Econf u e ((Rapp (Rvar Hole) (Rclosure v e)) : s)
transition (Econf (Rabs x t) e s) = do
  box <- newIORef Empty
  pure $ Cconf s (Rcache (Box box) (Rclosure (Rabs x t) e))
transition (Econf (Rvar (Num x)) e s) = do
  def <- newIORef $ Done $ Rvar $ Num x
  let b@(Box m) = Map.findWithDefault (Box def) x e
  rd <- readIORef m
  case rd of
    Todo (Rclosure v e') -> pure $ Econf v e' ((Rcache b (Rvar Hole)) : s)
    Done t               -> pure $ Cconf s t
    _                    -> invalidProgramState
transition (Cconf ((Rcache (Box m) (Rvar Hole)) : s) t) = do
  writeIORef m (Done t)
  pure $ Cconf s t
transition (Cconf ((Rapp (Rvar Hole) ve) : s) (Rcache _ (Rclosure (Rabs x t) e)))
  = do
    box <- newIORef (Todo ve)
    pure $ Econf t (Map.insert x (Box box) e) s
transition (Cconf s (Rcache (Box m) (Rclosure (Rabs x t) e))) = do
  rd <- readIORef m
  case rd of
    Done v -> pure $ Cconf s v
    Empty  -> do
      x1  <- randomIO :: IO Int
      box <- newIORef $ Done $ Rvar $ Num x1
      pure $ Econf t
                   (Map.insert x (Box box) e)
                   ((Rabs x1 (Rvar Hole)) : (Rcache (Box m) (Rvar Hole)) : s)
    Todo _ -> invalidProgramState
transition (Cconf ((Rapp (Rvar Hole) (Rclosure v e)) : s) t) =
  pure $ Econf v e ((Rapp t (Rvar Hole)) : s)
transition (Cconf ((Rapp t (Rvar Hole)) : s) v) = pure $ Cconf s (Rapp t v)
transition (Cconf ((Rabs x1 (Rvar Hole)) : s) v) = pure $ Cconf s (Rabs x1 v)
transition (Cconf [] _) = pure End
transition _ = invalidProgramState

forEachState :: Conf -> (Conf -> IO Conf) -> IO Conf
forEachState conf trans = trans conf >>= \case
  End  -> pure conf
  next -> forEachState next trans

loadTerm :: Redex -> Conf
loadTerm t = Econf t Map.empty []

reduce :: Expression -> IO Expression
reduce e = do
  redex <- pure $ toRedex e
  forEachState (loadTerm redex) transition >>= \case
    Cconf [] v -> pure $ fromRedex v
    _          -> invalidProgramState
