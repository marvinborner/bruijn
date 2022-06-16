module Binary
  ( toBinary
  , fromBinary
  ) where

import           Control.Applicative
import           Data.Char
import           Helper

toBinary :: Expression -> String
toBinary (Bruijn      x        ) = (replicate x '1') ++ "0"
toBinary (Abstraction exp      ) = "00" ++ toBinary exp
toBinary (Application exp1 exp2) = "01" ++ (toBinary exp1) ++ (toBinary exp2)

-- Stolen from John Tromp
fromBinary :: String -> Expression
fromBinary = foldr
  (\x -> Abstraction . (Application . Application (Bruijn 0) . code $ x))
  nil
 where
  nil = code '1'
  code '0' = Abstraction (Abstraction (Bruijn 1))
  code '1' = Abstraction (Abstraction (Bruijn 0))
  code x   = fromBinary (showsBin 8 (ord x) "")
  showsBin n x = if n == 0
    then id
    else let (x', b) = divMod x 2 in showsBin (n - 1) x' . (intToDigit b :)
