-- MIT License, Copyright (c) 2022 Marvin Borner
module Conversion where

import qualified Data.BitString                as Bit
import qualified Data.ByteString.Lazy          as Byte
import qualified Data.ByteString.Lazy.Char8    as C
import           Data.Char                      ( chr )
import           GHC.Real                       ( denominator
                                                , numerator
                                                )
import           Numeric                        ( showFFloatAlt )

import           Helper

listify :: [Expression] -> Expression
listify [] = Abstraction (Abstraction (Bruijn 0))
listify (e : es) =
  Abstraction (Application (Application (Bruijn 0) e) (listify es))

binarify :: [Expression] -> Expression
binarify = foldr Application (Bruijn 2)

encodeByte :: [Bool] -> Expression
encodeByte bits = Abstraction $ Abstraction $ Abstraction $ binarify
  (map encodeBit bits)
 where
  encodeBit False = Bruijn 0
  encodeBit True  = Bruijn 1

-- TODO: There must be a better way to do this :D
encodeBytes :: Byte.ByteString -> Expression
encodeBytes bytes = listify $ map
  (encodeByte . Bit.toList . Bit.bitStringLazy . Byte.pack . (: []))
  (Byte.unpack bytes)

stringToExpression :: String -> Expression
stringToExpression = encodeBytes . C.pack

charToExpression :: Char -> Expression
charToExpression ch = encodeByte $ Bit.toList $ Bit.bitStringLazy $ C.pack [ch]

encodeStdin :: IO Expression
encodeStdin = encodeBytes <$> Byte.getContents

unlistify :: Expression -> Maybe [Expression]
unlistify (Abstraction (Abstraction (Bruijn 0))) = Just []
unlistify (Abstraction (Application (Application (Bruijn 0) e) es)) =
  (:) <$> Just e <*> unlistify es
unlistify _ = Nothing

unpairify :: Expression -> Maybe [Expression]
unpairify (Abstraction (Application (Application (Bruijn 0) e1) e2)) =
  Just (e1 : [e2])
unpairify _ = Nothing

decodeByte :: Expression -> Maybe [Bool]
decodeByte (Abstraction (Abstraction (Abstraction es))) = decodeByte es
decodeByte (Application (Bruijn 0) es) = (:) <$> Just False <*> decodeByte es
decodeByte (Application (Bruijn 1) es) = (:) <$> Just True <*> decodeByte es
decodeByte (Bruijn 2                 ) = Just []
decodeByte _                           = Nothing

decodeStdout :: Expression -> Maybe String
decodeStdout e = do
  u <- unlistify e
  pure $ C.unpack $ Byte.concat $ map
    (\m -> case decodeByte m of
      Just b  -> Bit.realizeBitStringLazy $ Bit.fromList b
      Nothing -> Byte.empty
    )
    u

---

floatToRational :: Rational -> Expression
floatToRational f = Abstraction
  (Application (Application (Bruijn 0) (decimalToTernary p))
               (decimalToTernary $ q - 1)
  )
 where
  p = numerator f
  q = denominator f

floatToReal :: Rational -> Expression
floatToReal = Abstraction . floatToRational

floatToComplex :: Rational -> Rational -> Expression
floatToComplex r i = Abstraction $ Abstraction $ Application
  (Application (Bruijn 0) (Application (floatToReal r) (Bruijn 1)))
  (Application (floatToReal i) (Bruijn 1))

-- Dec to Bal3 in Bruijn encoding: reversed application with 0=>0; 1=>1; T=>2; end=>3
-- e.g. 0=0=[[[[3]]]]; 2=1T=[[[[2 (1 3)]]]] -5=T11=[[[[1 (1 (2 3))]]]]
decimalToTernary :: Integer -> Expression
decimalToTernary n =
  Abstraction $ Abstraction $ Abstraction $ Abstraction $ gen n
 where
  gen 0 = Bruijn 3
  gen n' =
    Application (Bruijn $ fromIntegral $ mod n' 3) (gen $ div (n' + 1) 3)

-- Decimal to binary encoding
decimalToBinary :: Integer -> Expression
decimalToBinary n | n < 0     = decimalToBinary 0
                  | otherwise = Abstraction $ Abstraction $ Abstraction $ gen n
 where
  gen 0  = Bruijn 2
  gen n' = Application (Bruijn $ fromIntegral $ mod n' 2) (gen $ div n' 2)

-- Decimal to unary (church) encoding
decimalToUnary :: Integer -> Expression
decimalToUnary n | n < 0     = decimalToUnary 0
                 | otherwise = Abstraction $ Abstraction $ gen n
 where
  gen 0  = Bruijn 0
  gen n' = Application (Bruijn 1) (gen (n' - 1))

-- Decimal to de Bruijn encoding
decimalToDeBruijn :: Integer -> Expression
decimalToDeBruijn n | n < 0     = decimalToDeBruijn 0
                    | otherwise = gen n
 where
  gen 0  = Abstraction $ Bruijn $ fromInteger n
  gen n' = Abstraction $ gen (n' - 1)

unaryToDecimal :: Expression -> Maybe String
unaryToDecimal e = (<> "u") . show <$> unaryToDecimal' e

unaryToDecimal' :: Expression -> Maybe Integer
unaryToDecimal' e = do
  res <- resolve e
  return (sum res :: Integer)
 where
  multiplier (Bruijn 1) = Just 1
  multiplier _          = Nothing
  resolve' (Bruijn 0) = Just []
  resolve' (Application x@(Bruijn _) (Bruijn 0)) =
    (:) <$> multiplier x <*> Just []
  resolve' (Application x@(Bruijn _) xs@(Application _ _)) =
    (:) <$> multiplier x <*> resolve' xs
  resolve' _ = Nothing
  resolve (Abstraction (Abstraction n)) = resolve' n
  resolve _                             = Nothing

binaryToChar :: Expression -> Maybe String
binaryToChar e = show <$> binaryToChar' e

binaryToChar' :: Expression -> Maybe Char
binaryToChar' e = do
  n <- binaryToDecimal e
  if n > 31 && n < 127 || n == 10 then Just $ chr $ fromIntegral n else Nothing

binaryToString :: Expression -> Maybe String
binaryToString e = (<> "b") . show <$> binaryToDecimal e

binaryToDecimal :: Expression -> Maybe Integer
binaryToDecimal e = do
  res <- resolve e
  return (sum $ zipWith (*) res (iterate (* 2) 1) :: Integer)
 where
  multiplier (Bruijn 0) = Just 0
  multiplier (Bruijn 1) = Just 1
  multiplier _          = Nothing
  resolve' (Bruijn 2) = Just []
  resolve' (Application x@(Bruijn _) (Bruijn 2)) =
    (:) <$> multiplier x <*> Just []
  resolve' (Application x@(Bruijn _) xs@(Application _ _)) =
    (:) <$> multiplier x <*> resolve' xs
  resolve' _ = Nothing
  resolve (Abstraction (Abstraction (Abstraction n))) = resolve' n
  resolve _ = Nothing

ternaryToString :: Expression -> Maybe String
ternaryToString e = (<> "t") . show <$> ternaryToDecimal e

ternaryToDecimal :: Expression -> Maybe Integer
ternaryToDecimal e = do
  res <- resolve e
  return (sum $ zipWith (*) res (iterate (* 3) 1) :: Integer)
 where
  multiplier (Bruijn 0) = Just 0
  multiplier (Bruijn 1) = Just 1
  multiplier (Bruijn 2) = Just (-1)
  multiplier _          = Nothing
  resolve' (Bruijn 3) = Just []
  resolve' (Application x@(Bruijn _) (Bruijn 3)) =
    (:) <$> multiplier x <*> Just []
  resolve' (Application x@(Bruijn _) xs@(Application _ _)) =
    (:) <$> multiplier x <*> resolve' xs
  resolve' _ = Nothing
  resolve (Abstraction (Abstraction (Abstraction (Abstraction n)))) =
    resolve' n
  resolve _ = Nothing

rationalToString :: Expression -> Maybe String
rationalToString (Abstraction (Application (Application (Bruijn 0) a) b)) = do
  n <- ternaryToDecimal a
  d <- ternaryToDecimal b
  Just
    $  show n
    <> "/"
    <> show (d + 1)
    <> " (approx. "
    <> showFFloatAlt (Just 8)
                     (fromIntegral n / fromIntegral (d + 1) :: Double)
                     ""
    <> ")"
rationalToString _ = Nothing

realToString :: Expression -> Maybe String
realToString (Abstraction e) = rationalToString e
realToString _               = Nothing

complexToString :: Expression -> Maybe String
complexToString (Abstraction (Abstraction (Application (Application (Bruijn 0) (Abstraction (Application (Application (Bruijn 0) lr) rr))) (Abstraction (Application (Application (Bruijn 0) li) ri)))))
  = do
    nlr <- ternaryToDecimal lr
    drr <- ternaryToDecimal rr
    nli <- ternaryToDecimal li
    dri <- ternaryToDecimal ri
    Just
      $  show nlr
      <> "/"
      <> show (drr + 1)
      <> " + "
      <> show nli
      <> "/"
      <> show (dri + 1)
      <> "i"
      <> " (approx. "
      <> showFFloatAlt (Just 8)
                       (fromIntegral nlr / fromIntegral (drr + 1) :: Double)
                       ""
      <> "+"
      <> showFFloatAlt (Just 8)
                       (fromIntegral nli / fromIntegral (dri + 1) :: Double)
                       ""
      <> "i)"
complexToString _ = Nothing
