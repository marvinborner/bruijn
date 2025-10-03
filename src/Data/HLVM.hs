-- MIT License, Copyright (c) 2025 Marvin Borner
module Data.HLVM where

type MachineCode = [Int]

type Var = Int
type Label = Int

data Node f
  = Constructor {pp :: f, left :: f, right :: f}
  | Duplicator {pp :: f, left :: f, right :: f, label :: Label}
  | Eraser {pp :: f}
  | Actor {pp :: f}
  | Token {pp :: f}
  | Binder Var
