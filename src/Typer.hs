module Typer
  ( typeCheck
  ) where

import           Helper

-- removes ys from start of xs
dropList :: (Eq a) => [a] -> [a] -> [a]
dropList [] _  = []
dropList xs [] = xs
dropList e@(x : xs) (y : ys) | x == y    = dropList xs ys
                             | otherwise = e

typeMatches :: Type -> Type -> Bool
typeMatches AnyType _       = True
typeMatches _       AnyType = True
typeMatches a       b       = a == b

typeApply :: Type -> Type -> Type
typeApply (FunctionType ts1) (FunctionType ts2) =
  FunctionType $ dropList ts1 ts2
typeApply AnyType _       = AnyType
typeApply _       AnyType = AnyType
typeApply a       b       = error "invalid"

typeImply :: Expression -> Type
typeImply (Bruijn      _        ) = AnyType
typeImply (Abstraction e        ) = typeImply e
typeImply (Application     e1 e2) = typeApply (typeImply e1) (typeImply e2)
typeImply (TypedExpression t  _ ) = t
typeImply _                       = error "invalid"

typeCheck :: Expression -> Type -> Failable Expression
typeCheck e@(Bruijn   _) _ = Right e
typeCheck e@(Function _) _ = error "invalid"
typeCheck e@(Abstraction e') (FunctionType (t : ts)) =
  typeCheck e' (FunctionType ts) >>= pure (Right e)
typeCheck e@(Abstraction _) _ = Right e
typeCheck e@(Application e1 e2) t
  | typeMatches (typeImply e) t = Right e
  | otherwise = Left $ TypeError [(typeImply e1), (typeImply e2), t]
typeCheck e@(MixfixChain _) _ = Right e
typeCheck e@(Prefix _ _   ) _ = Right e
typeCheck (TypedExpression t' e) t | typeMatches t' t = Right e
                                   | otherwise        = Left $ TypeError [t', t]
