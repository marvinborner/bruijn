{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
-- extensions above are only used because of printBundle from megaparsec

module Helper where

import qualified Control.Monad.State           as S
import qualified Data.BitString                as Bit
import qualified Data.ByteString               as Byte
import qualified Data.ByteString.Char8         as C
import           Data.List
import qualified Data.Map                      as M
import           Text.Megaparsec

data Context = Context
  { _ctxInput :: String
  , _ctxPath  :: String
  }

printContext :: Context -> String
printContext (Context inp ""  ) = printContext (Context inp "<unknown>")
printContext (Context inp path) = p $ lines inp
 where
  withinText = "\ESC[42mwithin\ESC[0m "
  inText     = "\ESC[44min\ESC[0m "
  nearText   = "\ESC[45mnear\ESC[0m\n"
  p []  = withinText <> show path <> "\n"
  p [l] = inText <> show l <> "\n" <> withinText <> show path <> "\n"
  p (l : ls) =
    (p [l])
      <> nearText
      <> (intercalate "\n" $ map ("  | " ++) $ take 3 $ ls)
      <> "\n"

errPrefix :: String
errPrefix = "\ESC[41mERROR\ESC[0m "
data Error = SyntaxError String | UndeclaredIdentifier Identifier | InvalidIndex Int | FailedTest Expression Expression Expression Expression | ContextualError Error Context | ImportError String
instance Show Error where
  show (ContextualError err ctx) = show err <> "\n" <> (printContext ctx)
  show (SyntaxError err) =
    errPrefix <> "invalid syntax\n\ESC[45mnear\ESC[0m " <> err
  show (UndeclaredIdentifier ident) =
    errPrefix <> "undeclared identifier " <> show ident
  show (InvalidIndex err) = errPrefix <> "invalid index " <> show err
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
  show (ImportError path) = errPrefix <> "invalid import " <> show path
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
    outChunk       = "\n\n" <> offendingLine <> (init $ parseErrorTextPretty e)
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

data Identifier = NormalFunction String | InfixFunction String | PrefixFunction String | NamespacedFunction String Identifier
  deriving (Ord, Eq)
functionName :: Identifier -> String
functionName = \case
  NormalFunction f       -> f
  InfixFunction  i       -> "(" <> i <> ")"
  PrefixFunction p       -> p <> "("
  NamespacedFunction n f -> n <> functionName f
instance Show Identifier where
  show ident = "\ESC[95m" <> functionName ident <> "\ESC[0m"
data Expression = Bruijn Int | Function Identifier | Abstraction Expression | Application Expression Expression | Infix Expression Identifier Expression | Prefix Identifier Expression
  deriving (Ord, Eq)
data Instruction = Define Identifier Expression [Instruction] | Evaluate Expression | Comment | Input String | Import String String | Test Expression Expression | ContextualInstruction Instruction String
  deriving (Show)
instance Show Expression where
  show (Bruijn      x    ) = "\ESC[91m" <> show x <> "\ESC[0m"
  show (Function    ident) = "\ESC[95m" <> show ident <> "\ESC[0m"
  show (Abstraction e    ) = "\ESC[36m[\ESC[0m" <> show e <> "\ESC[36m]\ESC[0m"
  show (Application exp1 exp2) =
    "\ESC[33m(\ESC[0m" <> show exp1 <> " " <> show exp2 <> "\ESC[33m)\ESC[0m"
  show (Infix le i re) =
    "\ESC[33m(\ESC[0m"
      <> show i
      <> " "
      <> show le
      <> " "
      <> show re
      <> "\ESC[33m)\ESC[0m"
  show (Prefix p e) = show p <> " " <> show e

data EvalConf = EvalConf
  { _isRepl    :: Bool
  , _evalTests :: Bool
  , _nicePath  :: String
  , _evalPaths :: [String]
  }
data ExpFlags = ExpFlags
  { _isImported :: Bool
  }
  deriving Show
data EnvDef = EnvDef
  { _exp   :: Expression
  , _sub   :: Environment
  , _flags :: ExpFlags
  }
  deriving Show
data Environment = Environment (M.Map Identifier EnvDef)
  deriving Show
data EnvCache = EnvCache
  { _imported :: M.Map String Environment
  }
type EvalState = S.State Environment

defaultConf :: String -> EvalConf
defaultConf path = EvalConf { _isRepl    = False
                            , _evalTests = True
                            , _nicePath  = path
                            , _evalPaths = []
                            }

defaultFlags :: ExpFlags
defaultFlags = ExpFlags { _isImported = False }

---

listify :: [Expression] -> Expression
listify [] = Abstraction (Abstraction (Bruijn 0))
listify (e : es) =
  Abstraction (Application (Application (Bruijn 0) e) (listify es))

encodeByte :: [Bool] -> Expression
encodeByte bits = listify (map encodeBit bits)
 where
  encodeBit False = Abstraction (Abstraction (Bruijn 0))
  encodeBit True  = Abstraction (Abstraction (Bruijn 1))

-- TODO: There must be a better way to do this :D
encodeBytes :: Byte.ByteString -> Expression
encodeBytes bytes = listify $ map
  (encodeByte . Bit.toList . Bit.bitString . Byte.pack . (: []))
  (Byte.unpack bytes)

stringToExpression :: String -> Expression
stringToExpression = encodeBytes . C.pack

charToExpression :: Char -> Expression
charToExpression ch = encodeByte $ Bit.toList $ Bit.bitString $ C.pack [ch]

encodeStdin :: IO Expression
encodeStdin = do
  bytes <- Byte.getContents
  pure $ encodeBytes bytes

unlistify :: Expression -> Maybe [Expression]
unlistify (Abstraction (Abstraction (Bruijn 0))) = Just []
unlistify (Abstraction (Application (Application (Bruijn 0) e) es)) =
  (:) <$> Just e <*> (unlistify es)
unlistify _ = Nothing

decodeByte :: Expression -> Maybe [Bool]
decodeByte (Abstraction (Abstraction (Bruijn 0))) = Just []
decodeByte (Abstraction (Application (Application (Bruijn 0) (Abstraction (Abstraction (Bruijn 0)))) es))
  = (:) <$> Just False <*> (decodeByte es)
decodeByte (Abstraction (Application (Application (Bruijn 0) (Abstraction (Abstraction (Bruijn 1)))) es))
  = (:) <$> Just True <*> (decodeByte es)
decodeByte _ = Nothing

decodeStdout :: Expression -> Maybe String
decodeStdout e = do
  u <- unlistify e
  pure $ C.unpack $ Byte.concat $ map
    (\m -> case decodeByte m of
      Just b  -> Bit.realizeBitStringStrict $ Bit.fromList b
      Nothing -> Byte.empty
    )
    u

---

-- TODO: Performanize
matchingFunctions :: Expression -> Environment -> String
matchingFunctions e (Environment env) =
  intercalate ", " $ map (functionName . fst) $ M.toList $ M.filter
    (\EnvDef { _exp = e' } -> e == e')
    env

-- TODO: Expression -> Maybe Char is missing
maybeHumanifyExpression :: Expression -> Maybe String
maybeHumanifyExpression e = ternaryToDecimal e <|> decodeStdout e

humanifyExpression :: Expression -> String
humanifyExpression e = case maybeHumanifyExpression e of
  Nothing -> show e
  Just h  -> h

---

-- Dec to Bal3 in Bruijn encoding: reversed application with 0=>0; 1=>1; T=>2; end=>3
-- e.g. 0=0=[[[[3]]]]; 2=1T=[[[[2 (0 3)]]]] -5=T11=[[[[0 (0 (2 3))]]]]
decimalToTernary :: Integer -> Expression
decimalToTernary n =
  Abstraction $ Abstraction $ Abstraction $ Abstraction $ gen n
 where
  gen 0 = Bruijn 3
  gen n' =
    Application (Bruijn $ fromIntegral $ mod n' 3) (gen $ div (n' + 1) 3)

ternaryToDecimal :: Expression -> Maybe String
ternaryToDecimal e = do
  res <- resolve e
  return $ show $ (sum $ zipWith (*) res (iterate (* 3) 1) :: Integer)
 where
  multiplier (Bruijn 0) = Just 0
  multiplier (Bruijn 1) = Just 1
  multiplier (Bruijn 2) = Just (-1)
  multiplier _          = Nothing
  resolve' (Application x@(Bruijn _) (Bruijn 3)) =
    (:) <$> multiplier x <*> Just []
  resolve' (Application x@(Bruijn _) xs@(Application _ _)) =
    (:) <$> (multiplier x) <*> (resolve' xs)
  resolve' _ = Nothing
  resolve (Abstraction (Abstraction (Abstraction (Abstraction n)))) =
    resolve' n
  resolve _ = Nothing
