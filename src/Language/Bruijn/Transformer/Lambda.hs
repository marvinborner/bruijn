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
import           Language.Generic.Annotation    ( fakeAnn
                                                , pattern AnnF
                                                , pattern FixAnnF
                                                , mapAnnM
                                                )
import           Language.Generic.Error         ( Error(..)
                                                , ErrorT
                                                , MonadError
                                                , runErrorT
                                                , throwError
                                                )

import           Debug.Trace

data Context = Context
  { stk   :: [Identifier]
  , depth :: Int
  }

type Transform = ErrorT (State Context)

data Tree = Tree Identifier Tree TermAnn | Branch Identifier Tree Tree | Leaf TermAnn | End

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
treeify :: TermAnn -> Tree
treeify = flip go End
 where
  go :: TermAnn -> Tree -> Tree
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

transformTerm :: TermAnn -> Transform Lambda.TermAnn
transformTerm = mapAnnM $ \case
  AnnF a (AbstractionF term) -> do
    ctx@Context { depth = d } <- get
    put $ ctx { depth = d + 1 }
    term' <- transformTerm term
    put $ ctx { depth = d }
    return $ AnnF a $ Lambda.AbstractionF term'

  AnnF a (ApplicationF terms) ->
    AnnF a . Lambda.ApplicationF <$> sequenceA (transformTerm <$> terms)

  AnnF a (IndexF        i   ) -> return $ AnnF a $ Lambda.IndexF $ i

  AnnF a (SubstitutionF name) -> do
    Context { stk = s, depth = d } <- get
    let maybeIdx = elemIndex name s
    case maybeIdx of
      Just i -> return $ AnnF a $ Lambda.IndexF $ i + d
      Nothing ->
        throwError
          $  TransformError a
          $  "can not substitute: "
          <> T.pack (show name)
          <> ", depth: "
          <> T.pack (show d)
          <> ", in scope: "
          <> T.pack (show s)

  AnnF a ForceF -> return $ AnnF a Lambda.TokenF

  AnnF a termM ->
    throwError $ TransformError a $ "unexpected " <> T.pack (show termM)

-- TODO: when a term is open, it must be substituted immediately (do this in preprocessor?)
transformTree :: Tree -> Transform Lambda.TermAnn
transformTree (Tree name tree term) = do
  ctx@Context { stk = s } <- get
  term'                   <- transformTerm term
  put ctx { stk = name : s }
  tree' <- transformTree tree
  put ctx { stk = s }
  return $ fakeAnn $ Lambda.ApplicationF
    [fakeAnn $ Lambda.AbstractionF tree', term']
transformTree (Branch name a b) = do
  ctx@Context { stk = s } <- get
  b'                      <- transformTree b
  put ctx { stk = name : s }
  a' <- transformTree a
  put ctx { stk = s }
  return $ fakeAnn $ Lambda.ApplicationF [fakeAnn $ Lambda.AbstractionF a', b']
transformTree (Leaf term) = transformTerm term
transformTree End         = return $ fakeAnn $ Lambda.IndexF 0

transform :: (MonadError m) => TermAnn -> m Lambda.TermAnn
transform term = do
  let tree = treeify term
  let result = trace (show tree) $ evalState (runErrorT $ transformTree tree)
                                             Context { stk = [], depth = 0 }
  either throwError return result
