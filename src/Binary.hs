-- MIT License, Copyright (c) 2022 Marvin Borner
module Binary
  ( toBinary
  , fromBinary
  , toBitString
  , fromBitString
  ) where

import           Data.Binary                    ( decode
                                                , encode
                                                )
import qualified Data.BitString                as Bit
import           Data.Word                      ( Word8 )
import           Helper

toBinary :: Expression -> String
toBinary (Bruijn      x        ) = (replicate (x + 1) '1') ++ "0"
toBinary (Abstraction e        ) = "00" ++ toBinary e
toBinary (Application exp1 exp2) = "01" ++ (toBinary exp1) ++ (toBinary exp2)
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
    let idx = (length $ takeWhile (== '1') $ inp) - 1
    in  case rst of
          "" -> (Bruijn $ idx, "")
          _  -> (Bruijn $ idx, drop idx rst)

fromBinary :: String -> Expression
fromBinary = fst . fromBinary'

-- 1 byte indicating bit-padding at end + n bytes filled with bits
-- TODO: technically only 1 nibble is needed (use other nibble for versioning/sth?)
toBitString :: String -> Bit.BitString
toBitString str = Bit.concat
  [ Bit.bitStringLazy $ encode (fromIntegral $ length str `mod` 8 :: Word8)
  , Bit.fromList $ map
    (\case
      '0' -> False
      '1' -> True
      _   -> invalidProgramState
    )
    str
  ]

fromBitString :: Bit.BitString -> String
fromBitString bits =
  map
      (\case
        False -> '0'
        True  -> '1'
      )
    $ Bit.toList
    $ Bit.take (Bit.length bits - (fromIntegral $ pad bits))
    $ Bit.drop 8 bits
 where
  pad :: Bit.BitString -> Word8
  pad = decode . Bit.realizeBitStringLazy . Bit.take 8
