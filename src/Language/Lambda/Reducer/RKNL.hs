-- MIT License, Copyright (c) 2025 Marvin Borner
-- based on the RKNL abstract machine, extended with side effects
-- strict by monadic style, lazy by semantics

-- Plans:
-- - token-based IO (Haskell FFI)
-- - stepper/debugger (+ TUI UI)

module Language.Lambda.Reducer.RKNL (
  reduce,
) where

import Control.Concurrent.MVar
import Control.Monad.IO.Class (
  MonadIO,
  liftIO,
 )
import Data.Context (Context (..), phaseChange)
import Data.Fix (Fix (..))
import Data.Foreign (ForeignLanguage (..))
import Data.Lambda (
  TermAnn,
  TermF (..),
  alphaEquivalent,
 )
import Data.List (elemIndex)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Phase (Phase (BruijnToLambdaTransform, LambdaReduce))
import Data.Text (Text)
import Language.Generic.Annotation (
  AnnF,
  foldAnn,
  mapWithAnnM,
  pattern Ann,
 )
import Language.Generic.Error (
  Error (..),
  MonadError,
  throwError,
 )
import Language.Lambda.PrettyPrinter (prettyPrint)

type SourceTerm = TermAnn BruijnToLambdaTransform -- TODO: be more generic
type Term = TermAnn LambdaReduce
type PhaseContext = Context LambdaReduce
type PhaseError = MonadError (Error LambdaReduce)

type Store = Map Int Box
type Stack = [RedexAnn]

newtype NameGen = NameGen Int

data BoxValue = Todo RedexAnn | Done RedexAnn | Empty
newtype Box = Box (MVar BoxValue)
data Rvar = Num Int | Hole deriving (Show)

{- | Econf: machine evaluates a closure (RedexAnn) to a weak normal form
     Cconf: continues with already computed value
-}
data Conf = Econf NameGen RedexAnn Store Stack | Cconf NameGen Stack RedexAnn | End

data RedexF f
  = Rabs Int f
  | Rapp f f
  | Rvar Rvar
  | Rclosure f Store
  | Rcache Box f
  | Rtest f f
  | Rforeign ForeignLanguage Text
  | Rtoken
type RedexAnnF = AnnF PhaseContext RedexF
type RedexAnn = Fix RedexAnnF

invalidState :: (PhaseError m) => PhaseContext -> m a
invalidState a = throwError $ Error a "invalid machine state!"

nextName :: NameGen -> (Int, NameGen)
nextName (NameGen x) = (x, NameGen $ x + 1)

toRedex :: (PhaseError m) => Term -> m RedexAnn
toRedex = go (NameGen 1) []
 where
  go :: (PhaseError m) => NameGen -> [Int] -> Term -> m RedexAnn
  go g ns = \case
    Ann a (AbstractionF t) -> do
      let (v, g') = nextName g
      t' <- go g' (v : ns) t
      return $ Ann a $ Rabs v t'
    Ann a (ApplicationF ts) -> do
      ts' <- mapM (go g ns) ts
      return $ foldl1 (\l r -> Ann a $ Rapp l r) ts'
    Ann a (IndexF i) -> do
      let i' = if i < 0 || i >= length ns then i else ns !! i
      return $ Ann a $ Rvar $ Num i'
    Ann a (TestF l r) -> Ann a <$> (Rtest <$> go g ns l <*> go g ns r)
    Ann a (ForeignF l s) -> return $ Ann a $ Rforeign l s
    Ann a TokenF -> return $ Ann a $ Rtoken
    t@(Ann a _) -> throwError $ Error a $ "unexpected term " <> prettyPrint t

fromRedex :: (PhaseError m) => RedexAnn -> m Term
fromRedex = go []
 where
  go env = mapWithAnnM $ \ctx -> \case
    Rabs n t -> do
      t' <- go (n : env) t
      return $ AbstractionF t'
    Rapp l r -> do
      l' <- go env l
      r' <- go env r
      return $ ApplicationF [l', r']
    Rvar (Num n) -> return $ IndexF $ fromMaybe n (elemIndex n env)
    Rtest l r -> error "test"
    Rforeign l s -> return $ ForeignF l s
    Rtoken -> return TokenF
    _ -> throwError $ Error ctx "unexpected redex"

transition :: (PhaseError m, MonadIO m) => Conf -> m Conf
--- ECONF ---
transition (Econf g (Ann a hd) e s) = case hd of
  Rapp u v -> do
    return $
      Econf
        g
        u
        e
        (Ann a (Rapp (Ann a $ Rvar Hole) (Ann a $ Rclosure v e)) : s)
  Rabs x t -> do
    box <- liftIO $ newMVar Empty
    return $
      Cconf
        g
        s
        ( Ann a $
            Rcache (Box box) (Ann a $ Rclosure (Ann a $ Rabs x t) e)
        )
  Rvar (Num x) -> do
    def <- liftIO $ newMVar $ Done $ Ann a $ Rvar $ Num x
    let b@(Box m) = Map.findWithDefault (Box def) x e
    rd <- liftIO $ readMVar m
    case rd of
      Todo (Ann a' (Rclosure v e')) ->
        return $
          Econf g v e' (Ann a' (Rcache b (Ann a' $ Rvar Hole)) : s)
      Done t -> return $ Cconf g s t
      _ -> invalidState a
  Rtest l r -> do
    -- TODO: is s=[] correct here?
    l' <- reduceConf (Econf g l e []) >>= fromRedex
    r' <- reduceConf (Econf g r e []) >>= fromRedex
    let success = Ann a $ Rabs 1 (Ann a $ Rabs 0 (Ann a $ Rvar (Num 1)))
    let failure = Ann a $ Rabs 1 (Ann a $ Rabs 0 (Ann a $ Rvar (Num 0)))
    return $ Econf g (if l' `alphaEquivalent` r' then success else failure) e s
  Rforeign l b -> return $ Cconf g s $ Ann a hd -- TODO: should be econf because xi redex?
  -- Rforeign l b -> return $ Econf g (Ann a hd) e s -- just loops, obviously
  Rtoken -> return $ Cconf g s $ Ann a hd
  _ -> invalidState a
--- CCONF ---
transition (Cconf g (Ann a (Rcache (Box m) (Ann a' (Rvar Hole))) : s) t) = do
  liftIO $ modifyMVar_ m (\_ -> return $ Done t)
  return $ Cconf g s t
transition
  ( Cconf
      g
      (Ann a (Rapp (Ann _ (Rvar Hole)) ve) : s)
      (Ann b (Rcache _ (Ann _ (Rclosure (Ann _ (Rabs x t)) e))))
    ) = do
    box <- liftIO $ newMVar $ Todo ve
    return $ Econf g t (Map.insert x (Box box) e) s
transition -- must be exactly here
  ( Cconf
      g
      s
      (Ann a (Rcache (Box m) (Ann b (Rclosure (Ann c (Rabs x t)) e))))
    ) =
    do
      -- TODO: verify annotation-passing
      rd <- liftIO $ readMVar m
      case rd of
        Done v -> return $ Cconf g s v
        Empty -> do
          let (x1, g') = nextName g
          box <- liftIO $ newMVar $ Done $ Ann b $ Rvar $ Num x1
          return $
            Econf
              g'
              t
              (Map.insert x (Box box) e)
              ( Ann b (Rabs x1 $ Ann b $ Rvar Hole)
                  : Ann a (Rcache (Box m) $ Ann a $ Rvar Hole)
                  : s
              )
        Todo _ -> invalidState a
transition (Cconf g (Ann a hd : s) t@(Ann b t')) = case (hd, t') of
  (Rapp (Ann _ (Rvar Hole)) (Ann _ (Rclosure v e)), _) ->
    return $ Econf g v e $ Ann a (Rapp t $ Ann a $ Rvar Hole) : s
  (Rapp v (Ann _ (Rvar Hole)), _) ->
    return $ Cconf g s $ Ann a $ Rapp v t
  (Rabs x1 (Ann _ (Rvar Hole)), _) ->
    return $ Cconf g s $ Ann a $ Rabs x1 t
  _ -> invalidState a
transition (Cconf g [] _) = return End
transition _ = error "completely invalid machine state!"

forEachState :: (MonadIO m) => Conf -> (Conf -> m Conf) -> m Conf
forEachState conf trans =
  trans conf >>= \case
    End -> return conf
    next -> forEachState next trans

-- TODO: NameGen is arbitrary to not conflict with toRedex
loadTerm :: RedexAnn -> Conf
loadTerm t = Econf (NameGen 1000000) t Map.empty []

reduceConf :: (PhaseError m, MonadIO m) => Conf -> m RedexAnn
reduceConf conf =
  forEachState conf transition >>= \case
    Cconf _ [] v -> return v
    _ -> error "machine diverged"

reduce :: (PhaseError m, MonadIO m) => SourceTerm -> m Term
reduce e = do
  redex <- toRedex (foldAnn phaseChange e)
  reduceConf (loadTerm redex) >>= fromRedex
