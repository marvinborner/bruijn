{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
-- these extensions are only used because of printBundle from megaparsec

module Helper where

import qualified Control.Monad.State           as S
import qualified Data.BitString                as Bit
import qualified Data.ByteString               as Byte
import qualified Data.ByteString.Char8         as C
import           Data.List
import           Text.Megaparsec

data Context = Context
  { ctxInput :: String
  , ctxPath  :: String
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
data Error = SyntaxError String | UndeclaredFunction String | InvalidIndex Int | FailedTest Expression Expression Expression Expression | ContextualError Error Context | ImportError String
instance Show Error where
  show (ContextualError err ctx) = show err <> "\n" <> (printContext ctx)
  show (SyntaxError err) =
    errPrefix <> "invalid syntax\n\ESC[45mnear\ESC[0m " <> err
  show (UndeclaredFunction func) =
    errPrefix <> "undeclared function " <> show func
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

data Expression = Bruijn Int | Variable String | Abstraction Expression | Application Expression Expression
  deriving (Ord, Eq)
data Instruction = Define String Expression [Instruction] | Evaluate Expression | Comment | Import String String | Test Expression Expression | ContextualInstruction Instruction String
  deriving (Show)
instance Show Expression where
  show (Bruijn      x  ) = "\ESC[91m" <> show x <> "\ESC[0m"
  show (Variable    var) = "\ESC[95m" <> var <> "\ESC[0m"
  show (Abstraction e  ) = "\ESC[36m[\ESC[0m" <> show e <> "\ESC[36m]\ESC[0m"
  show (Application exp1 exp2) =
    "\ESC[33m(\ESC[0m" <> show exp1 <> " " <> show exp2 <> "\ESC[33m)\ESC[0m"

type EnvDef = (String, Expression)
-- TODO: Add EvalConf to EnvState?
data EvalConf = EvalConf
  { isRepl    :: Bool
  , nicePath  :: String
  , evalPaths :: [String]
  }
data Environment = Environment [(EnvDef, Environment)]
type Program = S.State Environment

instance Semigroup Environment where
  (Environment e1) <> (Environment e2) = Environment $ e1 <> e2

instance Show Environment where
  show (Environment env) = intercalate "\n" $ map
    (\((n, f), s) -> "\t" <> show n <> ": " <> show f <> " - " <> show s)
    env

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
  putStrLn "Waiting for stdin eof"
  bytes <- Byte.getContents
  pure $ encodeBytes bytes

unlistify :: Expression -> [Expression]
unlistify (Abstraction (Abstraction (Bruijn 0))) = []
unlistify (Abstraction (Application (Application (Bruijn 0) e) es)) =
  e : (unlistify es)
unlistify _ = error "invalid"

decodeByte :: Expression -> [Bool]
decodeByte (Abstraction (Abstraction (Bruijn 0))) = []
decodeByte (Abstraction (Application (Application (Bruijn 0) (Abstraction (Abstraction (Bruijn 0)))) es))
  = False : (decodeByte es)
decodeByte (Abstraction (Application (Application (Bruijn 0) (Abstraction (Abstraction (Bruijn 1)))) es))
  = True : (decodeByte es)
decodeByte _ = error "invalid" -- TODO: Better errors using Maybe

decodeStdout :: Expression -> String
decodeStdout e = C.unpack $ Byte.concat $ map
  (Bit.realizeBitStringStrict . Bit.fromList . decodeByte)
  (unlistify e)

---

likeTernary :: Expression -> Bool
likeTernary (Abstraction (Abstraction (Abstraction (Abstraction _)))) = True
likeTernary _ = False

-- Dec to Bal3 in Bruijn encoding: reversed application with 0=>0; 1=>1; T=>2; end=>3
-- e.g. 0=0=[[[[3]]]]; 2=1T=[[[[2 (0 3)]]]] -5=T11=[[[[0 (0 (2 3))]]]]
decimalToTernary :: Integer -> Expression
decimalToTernary n =
  Abstraction $ Abstraction $ Abstraction $ Abstraction $ gen n
 where
  gen 0 = Bruijn 3
  gen n' =
    Application (Bruijn $ fromIntegral $ mod n' 3) (gen $ div (n' + 1) 3)

ternaryToDecimal :: Expression -> Integer
ternaryToDecimal e = sum $ zipWith (*) (resolve e) (iterate (* 3) 1)
 where
  multiplier (Bruijn 0) = 0
  multiplier (Bruijn 1) = 1
  multiplier (Bruijn 2) = (-1)
  multiplier _          = 0 -- ??
  resolve' (Application x@(Bruijn _) (Bruijn 3)) = [multiplier x]
  resolve' (Application x@(Bruijn _) xs@(Application _ _)) =
    (multiplier x) : (resolve' xs)
  resolve' _ = [0]
  resolve (Abstraction (Abstraction (Abstraction (Abstraction n)))) =
    resolve' n
  resolve _ = [0]
