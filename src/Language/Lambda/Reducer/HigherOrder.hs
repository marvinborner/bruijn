-- MIT License, Copyright (c) 2025 Marvin Borner

module Language.Lambda.Reducer.HigherOrder
  (reduce) where

import           Data.Fix                       ( Fix(..))
import           Data.Lambda                    ( TermAnn
                                                , TermF(..)
                                                )
import           Data.List                      ( (!?) )
import Data.Context (Context(..), phaseChange)
import           Language.Generic.Annotation    ( fixAnnF
                                                , pattern FixAnnF
                                                , AnnF
                                                , foldAnn
                                                )
import Data.Phase (Phase(BruijnToLambdaTransform, LambdaReduce))

type SourceTerm = TermAnn BruijnToLambdaTransform -- TODO: be more generic
type Term = TermAnn LambdaReduce
type PhaseContext = Context LambdaReduce

type Symbol = Int

data NTermF t = NAbstractionF Symbol t | NApplicationF [t] | NVariableF Symbol | NTokenF | NCotokenF
type NTermAnnF = AnnF PhaseContext NTermF
type NTermAnn = Fix NTermAnnF

data HTermF t = HAbstractionF (t -> t) | HApplicationF t t | HVariableF Symbol | HTokenF | HCotokenF
type HTermAnnF = AnnF PhaseContext HTermF
type HTermAnn = Fix HTermAnnF

app :: HTermAnn -> HTermAnn -> HTermAnn
app (  FixAnnF _ (HAbstractionF f)) = f
app f@(FixAnnF a _                ) = fixAnnF a . HApplicationF f

-- I don't think we can make this "TermAnnF (m HTermAnn) -> m HTermAnn" for trace
-- because of monad in higher order
higher :: Term -> HTermAnn
higher = go []
 where
  go :: [HTermAnn] -> Term -> HTermAnn
  go env (FixAnnF a (IndexF i)) = case env !? i of
    Just i  -> i
    Nothing -> fixAnnF a $ HVariableF i
  go env (FixAnnF a (AbstractionF t)) =
    fixAnnF a $ HAbstractionF $ \x -> go (x : env) t
  go env (FixAnnF _ (ApplicationF ts)) = foldl1 app (go env <$> ts)
  go env (FixAnnF a TokenF) = fixAnnF a HTokenF
  go env (FixAnnF a CotokenF) = fixAnnF a HCotokenF

lower :: HTermAnn -> NTermAnn
lower = go 0
 where
  go :: Int -> HTermAnn -> NTermAnn
  go _ (FixAnnF a (HVariableF x)) = fixAnnF a $ NVariableF x
  go d (FixAnnF a (HAbstractionF t)) =
    fixAnnF a $ NAbstractionF d $ go (d + 1) (t (fixAnnF a $ HVariableF d))
  go d (FixAnnF a (HApplicationF l r)) =
    fixAnnF a $ NApplicationF [go d l, go d r]
  go _ (FixAnnF a HTokenF) = fixAnnF a NTokenF
  go _ (FixAnnF a HCotokenF) = fixAnnF a NCotokenF

fromNamed :: NTermAnn -> Term
fromNamed = go []
 where
  go :: [Symbol] -> NTermAnn -> Term
  go env (FixAnnF a (NVariableF x)) = fixAnnF a $ IndexF $ case env !? x of
    Just x  -> x
    Nothing -> x
  go env (FixAnnF a (NAbstractionF n t)) =
    fixAnnF a $ AbstractionF $ go (n : env) t
  go env (FixAnnF a (NApplicationF ts)) =
    fixAnnF a $ ApplicationF $ go env <$> ts
  go _ (FixAnnF a NTokenF) = fixAnnF a TokenF
  go _ (FixAnnF a NCotokenF) = fixAnnF a CotokenF

reduce :: SourceTerm -> Term
reduce = fromNamed . lower . higher . foldAnn phaseChange
