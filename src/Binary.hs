-- MIT License, Copyright (c) 2022 Marvin Borner
module Binary
  ( toBinary
  , fromBinary
  , toBitString
  , fromBitString
  , fromJot
  ) where

import qualified Data.BitString                as Bit

import           Helper

toBinary :: Expression -> String
toBinary (Bruijn      x        ) = replicate (x + 1) '1' ++ "0"
toBinary (Abstraction e        ) = "00" ++ toBinary e
toBinary (Application exp1 exp2) = "01" ++ toBinary exp1 ++ toBinary exp2
toBinary _                       = invalidProgramState

fromBinary' :: String -> (Expression, String)
fromBinary' inp = case inp of
  '0' : '0' : rst -> let (e, es) = fromBinary' rst in (Abstraction e, es)
  '0' : '1' : rst ->
    let (exp1, rst1) = fromBinary' rst
        (exp2, rst2) = fromBinary' rst1
    in  (Application exp1 exp2, rst2)
  '1' : _ : rst -> binaryBruijn rst
  _             -> invalidProgramState
 where
  binaryBruijn rst =
    let idx = length (takeWhile (== '1') inp) - 1
    in  case rst of
          "" -> (Bruijn idx, "")
          _  -> (Bruijn idx, drop idx rst)

fromBinary :: String -> Expression
fromBinary = fst . fromBinary'

toBitString :: String -> Bit.BitString
toBitString = Bit.fromList . map
  (\case
    '0' -> False
    '1' -> True
    _   -> invalidProgramState
  )

fromBitString :: Bit.BitString -> String
fromBitString =
  map
      (\case
        False -> '0'
        True  -> '1'
      )
    . Bit.toList

---

fromJot :: String -> Expression
fromJot = worker . reverse
 where
  s = Function $ NormalFunction "s"
  k = Function $ NormalFunction "k"
  worker ('0' : xs) = Application (Application (worker xs) s) k
  worker ('1' : xs) = Application s (Application k (worker xs))
  worker _          = Abstraction (Bruijn 0)
