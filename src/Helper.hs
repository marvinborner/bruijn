module Helper where

import           Control.Monad.State
import qualified Data.BitString                as Bit
import qualified Data.ByteString               as Byte
import           Data.List

printContext :: String -> String
printContext str = p $ lines str
 where
  p [l] = "in \"" <> l <> "\"\n"
  p (l : ls) =
    (p [l])
      <> "near\n"
      <> (intercalate "\n" $ map ("  | " ++) $ take 3 $ ls)
      <> "\n"
  p _ = ""

errPrefix :: String
errPrefix = "\ESC[41mERROR\ESC[0m "
data Error = SyntaxError String | UndeclaredFunction String | InvalidIndex Int | FailedTest Expression Expression Expression Expression | ContextualError Error String | ImportError String
instance Show Error where
  show (ContextualError err ctx) = show err <> "\n" <> (printContext ctx)
  show (SyntaxError err        ) = errPrefix <> "invalid syntax\nnear " <> err
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

data Expression = Bruijn Int | Variable String | Abstraction Expression | Application Expression Expression
  deriving (Ord, Eq)
data Instruction = Define String Expression [Instruction] String | Evaluate Expression | Comment | Import String String | Test Expression Expression
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
  , evalPaths :: [String]
  }
data Environment = Environment [(EnvDef, Environment)]
type Program = State Environment

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

encodeByte :: Bit.BitString -> Expression
encodeByte bits = listify (map encodeBit (Bit.toList bits))
 where
  encodeBit False = Abstraction (Abstraction (Bruijn 0))
  encodeBit True  = Abstraction (Abstraction (Bruijn 1))

encodeBytes :: Byte.ByteString -> Expression
encodeBytes bytes =
  listify (map (encodeByte . Bit.from01List . (: [])) (Byte.unpack bytes))

encodeStdin :: IO Expression
encodeStdin = do
  bytes <- Byte.getContents
  pure $ encodeBytes bytes

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
