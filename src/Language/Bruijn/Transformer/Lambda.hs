-- MIT License, Copyright (c) 2025 Marvin Borner

module Language.Bruijn.Transformer.Lambda
  ( transform
  ) where

import           Control.Monad.State            ( State
                                                , evalState
                                                , get
                                                , put
                                                )
import           Data.Bruijn                    ( Identifier(..)
                                                , TermAnn
                                                , TermF(..)
                                                )
import qualified Data.Lambda                   as Lambda
                                                ( TermAnn
                                                , TermF(..)
                                                )
import           Data.List                      ( elemIndex )
import qualified Data.Text                     as T
import           Language.Bruijn.PrettyPrinter  ( prettyPrint )
import           Language.Generic.Annotation    ( fakeContext
                                                , pattern FixAnnF
                                                , mapWithAnnM
                                                , foldAnn
                                                )
import           Language.Generic.Error         ( Error(..)
                                                , MonadError
                                                , ExceptT
                                                , runExceptT
                                                , throwError
                                                )

import           Debug.Trace
import Data.Context (phaseChange)
import Data.Phase (Phase(BruijnPreprocess, BruijnToLambdaTransform))

data TransformingContext = TransformingContext
  { stk   :: [Identifier]
  , depth :: Int
  }
type PhaseError = MonadError (Error BruijnToLambdaTransform)
type Transform = ExceptT (Error BruijnToLambdaTransform) (State TransformingContext)

type SourceTerm = TermAnn BruijnPreprocess
type Term = TermAnn BruijnToLambdaTransform
type TargetTerm = Lambda.TermAnn BruijnToLambdaTransform

data Tree = Tree Identifier Tree Term | Branch Identifier Tree Tree | Leaf Term | End

instance Show Tree where
  show (Tree name next term) =
    "((\\"
      <> show name
      <> " -> "
      <> show next
      <> ") "
      <> T.unpack (prettyPrint term)
      <> ")"
  show (Branch name left right) =
    "((\\" <> show name <> " -> " <> show left <> ") " <> show right <> ")"
  show (Leaf term) = T.unpack $ prettyPrint term
  show End         = "END"

-- | Turn definitions into a Tree (~ topological sort)
treeify :: Term -> Tree
treeify = flip go End
 where
  go :: Term -> Tree -> Tree
  go = \case
    FixAnnF _ (DefinitionF name term sub next) -> do
      let sub'  = go sub
      let next' = go next
      \k -> Branch name (next' k) (sub' (Leaf term))
    FixAnnF _ (DoF term sub next) -> do
      let sub'  = go sub
      let next' = go next
      \k -> Branch (Local "") (next' k) (sub' (Leaf term))
    FixAnnF _ EmptyF -> id
    t                -> error $ T.unpack $ prettyPrint t -- should be impossible by now

transformTerm :: Term -> Transform TargetTerm
transformTerm = mapWithAnnM $ \ann -> \case
  AbstractionF term -> do
    ctx@TransformingContext { depth = d } <- get
    put $ ctx { depth = d + 1 }
    term' <- transformTerm term
    put $ ctx { depth = d }
    return $ Lambda.AbstractionF term'

  ApplicationF terms ->
    Lambda.ApplicationF <$> sequenceA (transformTerm <$> terms)

  IndexF        i    -> return $ Lambda.IndexF $ i

  SubstitutionF name -> do
    TransformingContext { stk = s, depth = d } <- get
    let maybeIdx = elemIndex name s
    case maybeIdx of
      Just i -> return $ Lambda.IndexF $ i + d
      Nothing ->
        throwError
          $  Error ann
          $  "can not substitute: "
          <> T.pack (show name)
          <> ", depth: "
          <> T.pack (show d)
          <> ", in scope: "
          <> T.pack (show s)

  ForceF -> return $ Lambda.TokenF

  -- TODO???
  -- term -> throwError $ Error ann $ "unexpected " <> T.pack (show term)

-- TODO: when a term is open, it must be substituted immediately (do this in preprocessor?)
--       we could also do this with terms in NF (sharing doesn't help here anyway)
transformTree :: Tree -> Transform TargetTerm
transformTree (Tree name tree term) = do
  ctx@TransformingContext { stk = s } <- get
  term'                   <- transformTerm term
  put ctx { stk = name : s }
  tree' <- transformTree tree
  put ctx { stk = s }
  return $ fakeContext $ Lambda.ApplicationF
    [fakeContext $ Lambda.AbstractionF tree', term']
transformTree (Branch name a b) = do
  ctx@TransformingContext { stk = s } <- get
  b'                      <- transformTree b
  put ctx { stk = name : s }
  a' <- transformTree a
  put ctx { stk = s }
  return $ fakeContext $ Lambda.ApplicationF [fakeContext $ Lambda.AbstractionF a', b']
transformTree (Leaf term) = transformTerm term
transformTree End         = return $ fakeContext $ Lambda.IndexF 0

transform :: PhaseError m => SourceTerm -> m TargetTerm
transform term = do
  let term' = foldAnn phaseChange term
  let tree = treeify term'
  let result = trace (show tree) $ evalState (runExceptT $ transformTree tree)
                                             TransformingContext { stk = [], depth = 0 }
  either throwError return result
