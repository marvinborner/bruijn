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

data Context = Context
  { stk :: [Identifier]
  }

type Transform = ErrorT (State Context)

data Linear = Linear Identifier Linear TermAnn | End

instance Show Linear where
  show (Linear name next term) =
    "((\\"
      <> show name
      <> " -> "
      <> show next
      <> ") "
      <> T.unpack (prettyPrint term)
      <> ")"
  show End = "END"

-- | Turn definitions into a Linear (topological sort)
linearize :: TermAnn -> Linear
linearize = flip go End
 where
  go :: TermAnn -> Linear -> Linear
  go = \case
    Fix (AnnF _ (DefinitionF name term sub next)) -> do
      let sub'  = go sub
      let next' = go next
      sub' . \k -> Linear name (next' k) term
    Fix (AnnF _ (DoF term sub next)) -> do
      let sub'  = go sub
      let next' = go next
      sub' . \k -> Linear (Local "_") (next' k) term
    Fix (AnnF _ EmptyF) -> id
    t                   -> error $ T.unpack $ prettyPrint t -- should be impossible by now

transformTerm :: TermAnn -> Transform Lambda.TermAnn
transformTerm = mapTermAnnM $ \case
  AnnF a (AbstractionF term) ->
    AnnF a . Lambda.AbstractionF <$> transformTerm term

  AnnF a (ApplicationF terms) ->
    AnnF a . Lambda.ApplicationF <$> sequenceA (transformTerm <$> terms)

  AnnF a (IndexF        i   ) -> return $ AnnF a $ Lambda.IndexF i

  AnnF a (SubstitutionF name) -> do
    Context { stk = s } <- get
    let maybeIdx = elemIndex name s
    case maybeIdx of
      Just i -> return $ AnnF a $ Lambda.IndexF i
      Nothing ->
        throwError
          $  TransformError a
          $  "can not substitute: "
          <> T.pack (show name)
          <> ", available: "
          <> T.pack (show s)

  AnnF a ForceF -> return $ AnnF a Lambda.TokenF

  AnnF a termM ->
    throwError $ TransformError a $ "unexpected " <> T.pack (show termM)

transformLinear :: Linear -> Transform Lambda.TermAnn
transformLinear (Linear name lin term) = do
  ctx@Context { stk = s } <- get
  term'                   <- transformTerm term
  put ctx { stk = name : s }
  lin' <- transformLinear lin
  return $ fakeAnn $ Lambda.ApplicationF
    [fakeAnn $ Lambda.AbstractionF lin', term']
transformLinear End = return $ fakeAnn $ Lambda.IndexF 0

transform :: (MonadError m) => TermAnn -> m Lambda.TermAnn
transform term = do
  let lin    = linearize term
  let result = evalState (runErrorT $ transformLinear lin) Context { stk = [] }
  either throwError return result
