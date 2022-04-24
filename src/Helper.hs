module Helper where

import           Control.Monad.State
import           Text.Parsec             hiding ( State )

data Error = SyntaxError ParseError | UndeclaredFunction String | DuplicateFunction String | InvalidIndex Int | FatalError String
instance Show Error where
  show (SyntaxError        err) = show err
  show (UndeclaredFunction err) = "ERROR: undeclared function " <> show err
  show (DuplicateFunction  err) = "ERROR: duplicate function " <> show err
  show (InvalidIndex       err) = "ERROR: invalid index " <> show err
  show (FatalError         err) = show err
type Failable = Either Error

data Expression = Bruijn Int | Variable String | Abstraction Expression | Application Expression Expression
  deriving (Ord, Eq)
data Instruction = Define String Expression | Evaluate Expression | Comment String | Load String | Test Expression Expression
  deriving (Show)
instance Show Expression where
  show (Bruijn      x        ) = show x
  show (Variable    var      ) = var
  show (Abstraction exp      ) = "[" <> show exp <> "]"
  show (Application exp1 exp2) = "(" <> show exp1 <> " " <> show exp2 <> ")"

type Environment = [(String, Expression)]
type Program = State Environment

-- Dec to Bal3 in Bruijn encoding: reversed application with 1=>0; 0=>1; T=>2; end=>3
-- e.g. 0=0=[[[[3]]]]; 2=1T=[[[[2 (0 3)]]]] -5=T11=[[[[0 (0 (2 3))]]]]
decimalToTernary :: Integer -> Expression
decimalToTernary n =
  Abstraction $ Abstraction $ Abstraction $ Abstraction $ gen n
 where -- TODO: Consider switching 0 and 1 for better readability
  fix 0 = 1
  fix 1 = 0
  fix 2 = 2
  gen 0 = Bruijn 3
  gen n = Application (Bruijn $ fix $ mod n 3) (gen $ div (n + 1) 3)

ternaryToDecimal :: Expression -> Integer
ternaryToDecimal exp = sum $ zipWith (*) (resolve exp) (iterate (* 3) 1)
 where
  multiplier (Bruijn 0) = 1
  multiplier (Bruijn 1) = 0
  multiplier (Bruijn 2) = (-1)
  resolve' (Application x@(Bruijn _) (Bruijn 3)) = [multiplier x]
  resolve' (Application fst@(Bruijn _) rst@(Application _ _)) =
    (multiplier fst) : (resolve' rst)
  resolve' _ = [0]
  resolve (Abstraction (Abstraction (Abstraction (Abstraction n)))) =
    resolve' n
  resolve _ = [0]
