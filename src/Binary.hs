module Binary
  ( toBinary
  , fromBinary
  , toBitString
  , fromBitString
  ) where

import           Control.Applicative
import           Data.Binary                    ( decode
                                                , encode
                                                )
import qualified Data.BitString                as Bit
import qualified Data.ByteString.Lazy          as Byte
import           Data.Char
import           Data.Int                       ( Int8 )
import           Helper

toBinary :: Expression -> String
toBinary (Bruijn      x        ) = (replicate (x + 1) '1') ++ "0"
toBinary (Abstraction exp      ) = "00" ++ toBinary exp
toBinary (Application exp1 exp2) = "01" ++ (toBinary exp1) ++ (toBinary exp2)

-- Stolen from John Tromp
-- fromBinary :: String -> Expression
-- fromBinary = foldr
--   (\x -> Abstraction . (Application . Application (Bruijn 0) . code $ x))
--   nil
--  where
--   nil = code '1'
--   code '0' = Abstraction (Abstraction (Bruijn 1))
--   code '1' = Abstraction (Abstraction (Bruijn 0))
--   code x   = fromBinary (showsBin 8 (ord x) "")
--   showsBin n x = if n == 0
--     then id
--     else let (x', b) = divMod x 2 in showsBin (n - 1) x' . (intToDigit b :)

-- https://github.com/ljedrz/blc/blob/master/src/encoding/binary.rs
fromBinary' :: String -> (Expression, String)
fromBinary' = \case
  -- "" -> (Bruijn 0, "")
  '0' : '0' : rst ->
    let (exp, rst) = fromBinary' rst in (Abstraction exp, rst)
  '0' : '1' : rst ->
    let (exp1, rst1) = fromBinary' rst
        (exp2, rst2) = fromBinary' rst1
    in  (Application exp1 exp2, rst2)
  '1' : '0' : rst -> binaryBruijn rst
  '1' : '1' : rst -> binaryBruijn rst
  _               -> (Bruijn (-1), "")
 where
  binaryBruijn rst =
    let idx = length . takeWhile (== '1')
    in  case length rst of
          0 -> (Bruijn $ idx rst, "")
          _ -> (Bruijn $ idx rst, drop ((idx rst) + 1) rst)

fromBinary :: String -> Expression
fromBinary = fst . fromBinary'

-- 1 byte indicating bit-padding at end + n bytes filled with bits
-- TODO: technically only 1 nibble is needed (versioning/sth?)
toBitString :: String -> Bit.BitString
toBitString str = Bit.concat
  [ Bit.bitString $ Byte.toStrict $ encode
    (fromIntegral $ length str `mod` 8 :: Int8)
  , Bit.fromList $ map
    (\case
      '0' -> False
      '1' -> True
    )
    str
  ]

-- TODO: Fix this
fromBitString :: Bit.BitString -> String
fromBitString bits =
  map
      (\case
        False -> '0'
        True  -> '1'
      )
    $ Bit.toList
    $ Bit.take (Bit.length bits - pad bits)
    $ Bit.drop 8 bits
  where pad bits = decode $ Bit.realizeBitStringLazy $ Bit.take 8 bits
