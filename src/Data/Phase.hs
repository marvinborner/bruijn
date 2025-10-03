-- MIT License, Copyright (c) 2025 Marvin Borner
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Data.Phase where

data Phase
  = LambdaParse
  | LambdaReduce
  | BruijnParse
  | BruijnPreprocess
  | BruijnToLambdaTransform

-- reflect phase types to values
data SPhase (ph :: Phase) where
  SLambdaParse :: SPhase LambdaParse
  SLambdaReduce :: SPhase LambdaReduce
  SBruijnParse :: SPhase BruijnParse
  SBruijnPreprocess :: SPhase BruijnPreprocess
  SBruijnToLambdaTransform :: SPhase BruijnToLambdaTransform

instance Show (SPhase ph) where
  show SLambdaParse = "parsing lambda"
  show SLambdaReduce = "reducing lambda"
  show SBruijnParse = "parsing bruijn"
  show SBruijnPreprocess = "preprocessing bruijn"
  show SBruijnToLambdaTransform = "transforming bruijn to lambda"

instance HasSPhase LambdaParse where
  sphase = SLambdaParse
instance HasSPhase LambdaReduce where
  sphase = SLambdaReduce
instance HasSPhase BruijnParse where
  sphase = SBruijnParse
instance HasSPhase BruijnPreprocess where
  sphase = SBruijnPreprocess
instance HasSPhase BruijnToLambdaTransform where
  sphase = SBruijnToLambdaTransform

class HasSPhase ph where
  sphase :: SPhase ph
