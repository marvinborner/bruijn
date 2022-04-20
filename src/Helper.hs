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
data Instruction = Define String Expression | Evaluate Expression | Comment String | Load String
  deriving (Show)
instance Show Expression where
  show (Bruijn      x        ) = show x
  show (Variable    var      ) = show var
  show (Abstraction exp      ) = "[" <> show exp <> "]"
  show (Application exp1 exp2) = "(" <> show exp1 <> " " <> show exp2 <> ")"

type Environment = [(String, Expression)]
type Program = State Environment
