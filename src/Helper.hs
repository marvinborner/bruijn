module Helper where

import           Control.Monad.State

data Error = UndeclaredFunction String | DuplicateFunction String | InvalidIndex Int | FatalError String
instance Show Error where
  show (UndeclaredFunction err) = "ERROR: undeclared function " <> show err
  show (DuplicateFunction  err) = "ERROR: duplicate function " <> show err
  show (InvalidIndex       err) = "ERROR: invalid index " <> show err
  show (FatalError         err) = show err
type Failable = Either Error

data Expression = Bruijn Int | Variable String | Abstraction Expression | Application Expression Expression
  deriving (Ord, Eq)
data Instruction = Define String Expression | Evaluate Expression | Comment String | Import String | Test Expression Expression
  deriving (Show)
instance Show Expression where
  show (Bruijn      x        ) = show x
  show (Variable    var      ) = var
  show (Abstraction exp      ) = "[" <> show exp <> "]"
  show (Application exp1 exp2) = "(" <> show exp1 <> " " <> show exp2 <> ")"

type Environment = [(String, Expression)]
type Program = State Environment

decimalToBinary :: Integer -> Expression
decimalToBinary n = Abstraction $ Abstraction $ Abstraction $ Abstraction $ gen
  n
 where -- TODO: Consider switching 0 and 1 for better readability
  fix 0 = 1
  fix 1 = 0
  gen 0 = Bruijn 3
  gen 1 = Application (Bruijn 0) (gen 0)
  gen n | n < 0     = Application (Bruijn 2) (gen (-n))
        | otherwise = Application (Bruijn $ fix $ mod n 2) (gen $ div n 2)

binaryToDecimal :: Expression -> Integer
binaryToDecimal exp = sum $ zipWith (*) (resolve exp) (iterate (* 2) 1)
 where
  multiplier (Bruijn 0) = 1
  multiplier (Bruijn 1) = 0
  multiplier (Bruijn 2) = -1 -- TODO
  resolve' (Application x@(Bruijn _) (Bruijn 3)) = [multiplier x]
  resolve' (Application fst@(Bruijn _) rst@(Application _ _)) =
    (multiplier fst) : (resolve' rst)
  resolve' _ = [0]
  resolve (Abstraction (Abstraction (Abstraction (Abstraction n)))) =
    resolve' n
  resolve _ = [0]
