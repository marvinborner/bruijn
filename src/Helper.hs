-- MIT License, Copyright (c) 2022 Marvin Borner
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Helper where

import           Control.DeepSeq                ( NFData )
import qualified Control.Monad.State           as S
import           Data.Array
import           Data.List                      ( intercalate )
import qualified Data.Map                      as M
import           GHC.Generics                   ( Generic )

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

defaultFlags :: ExpFlags
defaultFlags = ExpFlags { _isImported = False }

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
