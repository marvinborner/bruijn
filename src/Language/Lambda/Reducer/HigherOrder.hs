-- MIT License, Copyright (c) 2025 Marvin Borner

module Language.Lambda.Reducer.HigherOrder (reduce) where

import Data.Context (Context (..), phaseChange)
import Data.Fix (Fix (..))
import Data.Lambda (
  TermAnn,
  TermF (..),
 )
import Data.List ((!?))
import Data.Maybe (fromMaybe)
import Data.Phase (Phase (BruijnToLambdaTransform, LambdaReduce))
import Language.Generic.Annotation (
  AnnF,
  fixAnnF,
  foldAnn,
  pattern Ann,
 )

type SourceTerm = TermAnn BruijnToLambdaTransform -- TODO: be more generic
type Term = TermAnn LambdaReduce
type PhaseContext = Context LambdaReduce

type Symbol = Int

data NTermF t
  = NAbstractionF Symbol t
  | NApplicationF [t]
  | NVariableF Symbol
  | NTokenF
  | NCotokenF
type NTermAnnF = AnnF PhaseContext NTermF
type NTermAnn = Fix NTermAnnF

data HTermF t
  = HAbstractionF (t -> t)
  | HApplicationF t t
  | HVariableF Symbol
  | HTokenF
  | HCotokenF
type HTermAnnF = AnnF PhaseContext HTermF
type HTermAnn = Fix HTermAnnF

app :: HTermAnn -> HTermAnn -> HTermAnn
app (Ann _ (HAbstractionF f)) = f
app f@(Ann a _) = fixAnnF a . HApplicationF f

-- I don't think we can make this "TermAnnF (m HTermAnn) -> m HTermAnn" for trace
-- because of monad in higher order
higher :: Term -> HTermAnn
higher = go []
 where
  go :: [HTermAnn] -> Term -> HTermAnn
  go env (Ann a (IndexF i)) = case env !? i of
    Just j -> j
    Nothing -> fixAnnF a $ HVariableF i
  go env (Ann a (AbstractionF t)) =
    fixAnnF a $ HAbstractionF $ \x -> go (x : env) t
  go env (Ann _ (ApplicationF ts)) = foldl1 app (go env <$> ts)
  go env (Ann a TokenF) = fixAnnF a HTokenF
  go env (Ann a CotokenF) = fixAnnF a HCotokenF

lower :: HTermAnn -> NTermAnn
lower = go 0
 where
  go :: Int -> HTermAnn -> NTermAnn
  go _ (Ann a (HVariableF x)) = fixAnnF a $ NVariableF x
  go d (Ann a (HAbstractionF t)) =
    fixAnnF a $ NAbstractionF d $ go (d + 1) (t (fixAnnF a $ HVariableF d))
  go d (Ann a (HApplicationF l r)) =
    fixAnnF a $ NApplicationF [go d l, go d r]
  go _ (Ann a HTokenF) = fixAnnF a NTokenF
  go _ (Ann a HCotokenF) = fixAnnF a NCotokenF

fromNamed :: NTermAnn -> Term
fromNamed = go []
 where
  go :: [Symbol] -> NTermAnn -> Term
  go env (Ann a (NVariableF x)) = fixAnnF a $ IndexF $ fromMaybe x (env !? x)
  go env (Ann a (NAbstractionF n t)) =
    fixAnnF a $ AbstractionF $ go (n : env) t
  go env (Ann a (NApplicationF ts)) =
    fixAnnF a $ ApplicationF $ go env <$> ts
  go _ (Ann a NTokenF) = fixAnnF a TokenF
  go _ (Ann a NCotokenF) = fixAnnF a CotokenF

reduce :: SourceTerm -> Term
reduce = fromNamed . lower . higher . foldAnn phaseChange
