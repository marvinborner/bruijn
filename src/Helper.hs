-- MIT License, Copyright (c) 2022 Marvin Borner
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Helper where

import           Control.DeepSeq                ( NFData )
import qualified Control.Monad.State           as S
import           Data.Array
import qualified Data.BitString                as Bit
import qualified Data.ByteString.Lazy          as Byte
import qualified Data.ByteString.Lazy.Char8    as C
import           Data.Char
import           Data.List
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromMaybe
                                                , isNothing
                                                )
import           GHC.Generics                   ( Generic )
import           GHC.Real                       ( denominator
                                                , numerator
                                                )
import           Numeric                        ( showFFloatAlt )
import           Text.Megaparsec

invalidProgramState :: a
invalidProgramState = error "invalid program state"

data Context = Context
  { _ctxInput :: String
  , _ctxPath  :: String
  }

printContext :: Context -> String
printContext (Context inp ""  ) = printContext (Context inp "<unknown>")
printContext (Context inp path) = p $ lines inp
 where
  withinText = "\ESC[106m\ESC[30mwithin\ESC[0m "
  inText     = "\ESC[104m\ESC[30min\ESC[0m "
  nearText   = "\ESC[105m\ESC[30mnear\ESC[0m\n"
  p []  = withinText <> show path <> "\n"
  p [l] = inText <> l <> "\n" <> withinText <> path <> "\n"
  p (l : ls) =
    p [l] <> nearText <> intercalate "\n" (map ("  | " ++) $ take 3 ls) <> "\n"

errPrefix :: String
errPrefix = "\ESC[101m\ESC[30mERROR\ESC[0m "

okPrefix :: String
okPrefix = "\ESC[102m\ESC[30m OK \ESC[0m "

data Error = SyntaxError String | UndefinedIdentifier Identifier | UnmatchedMixfix [MixfixIdentifierKind] [Mixfix] | InvalidIndex Int | FailedTest Expression Expression Expression Expression | PassedTest Expression Expression | ContextualError Error Context | SuggestSolution Error String | ImportError String | OptimizerError String

instance Show Error where
  show (ContextualError err ctx) = show err <> "\n" <> printContext ctx
  show (SuggestSolution err sol) =
    show err <> "\n\ESC[102m\ESC[30msuggestion\ESC[0m Perhaps you meant " <> sol
  show (SyntaxError err) =
    errPrefix <> "invalid syntax\n\ESC[105m\ESC[30mnear\ESC[0m " <> err
  show (UndefinedIdentifier ident) =
    errPrefix <> "undefined identifier " <> show ident
  show (UnmatchedMixfix ks ms) =
    errPrefix
      <> "couldn't find matching mixfix for "
      <> intercalate "" (map show ks)
      <> "\n\ESC[105m\ESC[30mnear\ESC[0m "
      <> unwords (map show ms)
  show (InvalidIndex err) = errPrefix <> "invalid index " <> show err
  show (PassedTest exp1 exp2) =
    okPrefix <> "test passed: " <> show exp1 <> " = " <> show exp2
  show (FailedTest exp1 exp2 red1 red2) =
    errPrefix
      <> "test failed: "
      <> show exp1
      <> " = "
      <> show exp2
      <> "\n      reduced to "
      <> show red1
      <> " = "
      <> show red2
  show (ImportError    path) = errPrefix <> "invalid import " <> show path
  show (OptimizerError msg ) = errPrefix <> "optimizer failed: " <> msg

type Failable = Either Error

-- Modified from megaparsec's errorBundlePretty
printBundle
  :: forall s e
   . (VisualStream s, TraversableStream s, ShowErrorComponent e)
  => ParseErrorBundle s e
  -> String
printBundle ParseErrorBundle {..} =
  let (r, _) = foldl f (id, bundlePosState) bundleErrors in drop 1 (r "")
 where
  f :: (ShowS, PosState s) -> ParseError s e -> (ShowS, PosState s)
  f (o, !pst) e = (o . (outChunk ++), pst')
   where
    (msline, pst') = reachOffset (errorOffset e) pst
    epos           = pstateSourcePos pst'
    outChunk       = "\n\n" <> offendingLine <> init (parseErrorTextPretty e)
    offendingLine  = case msline of
      Nothing -> ""
      Just sline ->
        let pointer    = "^"
            rpadding   = replicate rpshift ' '
            rpshift    = unPos (sourceColumn epos) - 2
            lineNumber = (show . unPos . sourceLine) epos
            padding    = replicate (length lineNumber + 1) ' '
        in  padding
              <> "|\n"
              <> "  | "
              <> sline
              <> "\n"
              <> padding
              <> "|  "
              <> rpadding
              <> pointer
              <> "\n"

data MixfixIdentifierKind = MixfixSome String | MixfixNone
  deriving (Ord, Eq, Generic, NFData)

instance Show MixfixIdentifierKind where -- don't colorize (due to map)
  show (MixfixSome e) = e
  show _              = "…"

data Identifier = NormalFunction String | MixfixFunction [MixfixIdentifierKind] | PrefixFunction String | NamespacedFunction String Identifier
  deriving (Ord, Eq, Generic, NFData)

functionName :: Identifier -> String
functionName = \case
  NormalFunction f       -> f
  MixfixFunction is      -> intercalate "" $ map show is
  PrefixFunction p       -> p <> "‣"
  NamespacedFunction n f -> n <> functionName f

instance Show Identifier where
  show ident = "\ESC[95m" <> functionName ident <> "\ESC[0m"

data Mixfix = MixfixOperator Identifier | MixfixExpression Expression
  deriving (Ord, Eq, Generic, NFData)

instance Show Mixfix where
  show (MixfixOperator   i) = show i
  show (MixfixExpression e) = show e

-- TODO: Remove Application and replace with Chain (renaming of MixfixChain)
data Expression = Bruijn Int | Function Identifier | Abstraction Expression | Application Expression Expression | MixfixChain [Mixfix] | Prefix Identifier Expression | Quote Expression | Unquote Expression
  deriving (Ord, Eq, Generic, NFData)

instance Show Expression where
  showsPrec _ (Bruijn x) =
    showString "\ESC[91m" . shows x . showString "\ESC[0m"
  showsPrec _ (Function ident) =
    showString "\ESC[95m" . shows ident . showString "\ESC[0m"
  showsPrec _ (Abstraction e) =
    showString "\ESC[36m[\ESC[0m" . shows e . showString "\ESC[36m]\ESC[0m"
  showsPrec _ (Application exp1 exp2) =
    showString "\ESC[33m(\ESC[0m"
      . shows exp1
      . showString " "
      . shows exp2
      . showString "\ESC[33m)\ESC[0m"
  showsPrec _ (MixfixChain [m]) =
    showString "\ESC[33m\ESC[0m" . shows m . showString "\ESC[33m\ESC[0m"
  showsPrec _ (MixfixChain ms) =
    showString "\ESC[33m(\ESC[0m"
      . foldr1 (\x y -> x . showString " " . y) (map shows ms)
      . showString "\ESC[33m)\ESC[0m"
  showsPrec _ (Prefix p e) =
    showString "\ESC[33m(\ESC[0m"
      . shows p
      . showString " "
      . shows e
      . showString "\ESC[33m)\ESC[0m"
  showsPrec _ (Quote   e) = showString "\ESC[36m`\ESC[0m" . shows e
  showsPrec _ (Unquote e) = showString "\ESC[36m,\ESC[0m" . shows e

data Command = Input String | Watch String | Import String String | Test Expression Expression | ClearState | Time Expression | Length Expression | Blc Expression | Jot String
  deriving (Show)

data Instruction = Define Identifier Expression [Instruction] | Evaluate Expression | Comment | Commands [Command] | ContextualInstruction Instruction String
  deriving (Show)

data ArgMode = ArgEval | ArgEvalBblc | ArgEvalBlc | ArgDumpBblc | ArgDumpBlc

data Args = Args
  { _argMode     :: ArgMode
  , _argNoTests  :: Bool
  , _argVerbose  :: Bool
  , _argOptimize :: Bool
  , _argToTarget :: String
  , _argReducer  :: String
  , _argPath     :: Maybe String
  }

data EvalConf = EvalConf
  { _isRepl    :: Bool
  , _isVerbose :: Bool
  , _evalTests :: Bool
  , _optimize  :: Bool
  , _nicePath  :: String
  , _path      :: String
  , _evalPaths :: [String]
  , _toTarget  :: String
  , _reducer   :: String
  , _hasArg    :: Bool
  }

newtype ExpFlags = ExpFlags
  { _isImported :: Bool
  }
  deriving (Show)

data EnvDef = EnvDef
  { _exp   :: Expression
  , _sub   :: Environment
  , _flags :: ExpFlags
  }
  deriving Show

newtype Environment = Environment (M.Map Identifier EnvDef)
  deriving (Show)

newtype EnvCache = EnvCache
  { _imported :: M.Map String Environment
  }

type EvalState = S.State Environment

argsToConf :: Args -> EvalConf
argsToConf args = EvalConf { _isRepl    = isNothing $ _argPath args
                           , _isVerbose = _argVerbose args
                           , _evalTests = not $ _argNoTests args
                           , _optimize  = _argOptimize args
                           , _path      = path
                           , _nicePath  = path
                           , _evalPaths = []
                           , _toTarget  = _argToTarget args
                           , _reducer   = _argReducer args
                           , _hasArg    = False
                           }
  where path = fromMaybe "" (_argPath args)

defaultFlags :: ExpFlags
defaultFlags = ExpFlags { _isImported = False }

---

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

-- from reddit u/cgibbard
levenshtein :: (Eq a) => [a] -> [a] -> Int
levenshtein xs ys = levMemo ! (n, m)
 where
  levMemo =
    array ((0, 0), (n, m)) [ ((i, j), lev i j) | i <- [0 .. n], j <- [0 .. m] ]
  n  = length xs
  m  = length ys
  xa = listArray (1, n) xs
  ya = listArray (1, m) ys
  lev 0 v = v
  lev u 0 = u
  lev u v
    | xa ! u == ya ! v = levMemo ! (u - 1, v - 1)
    | otherwise = 1 + minimum
      [levMemo ! (u, v - 1), levMemo ! (u - 1, v), levMemo ! (u - 1, v - 1)]

---

-- TODO: Performanize
matchingFunctions :: Expression -> Environment -> String
matchingFunctions e (Environment env) =
  intercalate ", " $ map (functionName . fst) $ M.toList $ M.filter
    (\EnvDef { _exp = e' } -> e == e')
    env

-- TODO: Show binary as char if in ascii range (=> + humanify strings)
-- TODO: Show list as pair if not ending with empty
maybeHumanifyExpression :: Expression -> Maybe String
maybeHumanifyExpression e =
  unaryToDecimal e
    <|> binaryToChar e
    <|> binaryToString e
    <|> ternaryToString e
    <|> rationalToString e
    <|> realToString e
    <|> complexToString e
    <|> humanifyString e
    <|> humanifyList e
    <|> humanifyPair e
    <|> humanifyMeta e

humanifyExpression :: Expression -> String
humanifyExpression e = fromMaybe "" (maybeHumanifyExpression e)

humanifyMeta :: Expression -> Maybe String
humanifyMeta e = ("`" <>) <$> go e
 where
  go (Abstraction (Abstraction (Abstraction (Application (Bruijn 0) t)))) =
    go t >>= (\a -> pure $ "[" <> a <> "]")
  go (Abstraction (Abstraction (Abstraction (Application (Application (Bruijn 1) a) b))))
    = go a >>= \l -> go b >>= \r -> pure $ "(" <> l <> " " <> r <> ")"
  go (Abstraction (Abstraction (Abstraction (Application (Bruijn 2) n)))) =
    fmap show (unaryToDecimal' n)
  go _ = Nothing

humanifyList :: Expression -> Maybe String
humanifyList e = do
  es <- unlistify e
  let conv x = fromMaybe (show x) (maybeHumanifyExpression x)
      m = map conv es
  pure $ "{" <> intercalate ", " m <> "}"

humanifyString :: Expression -> Maybe String
humanifyString e = do
  es  <- unlistify e
  str <- mapM binaryToChar' es
  pure $ "\"" <> str <> "\""

humanifyPair :: Expression -> Maybe String
humanifyPair e = do
  es <- unpairify e
  let conv x = fromMaybe (show x) (maybeHumanifyExpression x)
      m = map conv es
  pure $ "<" <> intercalate " : " m <> ">"

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
