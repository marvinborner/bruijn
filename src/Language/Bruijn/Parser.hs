-- MIT License, Copyright (c) 2025 Marvin Borner

module Language.Bruijn.Parser
  ( parse
  ) where

import           Control.Monad                  ( void )
import           Data.Bruijn                    ( Command(..)
                                                , Identifier(..)
                                                , Lambda(..)
                                                , MixfixIdentifier(..)
                                                , Name
                                                , SyntacticSugar(..)
                                                , Term(..)
                                                )
import           Data.Functor                   ( (<$) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Void
import           Text.Megaparsec         hiding ( State
                                                , parse
                                                )
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

data Cofree f a = a :> (f (Cofree f a))

-- data Located a = Located
--   { loc  :: SourcePos
--   , node :: a
--   }
--   deriving Eq

type Parser = Parsec Void Text

-- | single line comment
lineComment :: Parser ()
lineComment = L.skipLineComment "# "

-- | multiline comment
blockComment :: Parser ()
blockComment = L.skipBlockComment "###" "###"

-- | space consumer including comments and newlines
scn :: Parser ()
scn = L.space space1 lineComment blockComment

-- | space consumer including comments without newlines
sc :: Parser ()
sc = L.space (void $ some (oneOf (" \t" :: String))) lineComment blockComment

-- | lexeme consumer without newline
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | symbol consumer without newline
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | symbol consumer with newline
symbolN :: Text -> Parser Text
symbolN = L.symbol scn

upto1 :: Int -> Parser a -> Parser [a]
upto1 n p | n > 0 = (:) <$> p <*> upto1 (n - 1) p
upto1 _ _         = return []

namespace :: Parser Name
namespace = T.pack <$> ((:) <$> upperChar <*> many letterChar)

special :: Parser Name
special = T.pack <$> some alphaNumChar -- TODO!

local :: Parser Identifier
local = Local . T.pack <$> some alphaNumChar

namespaced :: Parser Identifier
namespaced = Namespaced <$> namespace <*> identifier

mixfix :: Parser Identifier
mixfix = Mixfix <$> many mixfixIdentifier
  where mixfixIdentifier = (Wildcard <$ char '…') <|> (Special <$> special)

identifier :: Parser Identifier
identifier = local <|> namespaced <|> mixfix

abstraction :: Parser Lambda
abstraction = Abstraction <$> (symbol "[" *> lexeme lambda <* symbol "]")

application :: Parser Lambda
application =
  Application <$> (symbol "(" *> some (lexeme singleton) <* symbol ")")

index :: Parser Lambda
index = Index . read . pure <$> digitChar

singleton :: Parser Lambda
singleton = lexeme $ abstraction <|> application <|> index

lambda :: Parser Lambda
lambda = Application <$> some (lexeme singleton)

definition :: Parser (Term -> Term -> Term)
definition = Definition <$> lexeme identifier <*> lexeme lambda

command :: Parser Command
command = char ':' *> test
 where
  test =
    Test
      <$> (symbol "test" *> symbol "(" *> lexeme lambda <* symbol ")")
      <*> (symbol "(" *> lexeme lambda <* symbol ")")

preprocessor :: Parser (Term -> Term -> Term)
preprocessor = Preprocessor <$> lexeme command

program :: Parser Term
program = do
  indent <- scn *> L.indentLevel
  instr  <- try definition <|> preprocessor
  sub    <- try (L.indentGuard scn GT indent *> program) <|> (Empty <$ scn)
  next   <- try (L.indentGuard scn EQ indent *> program) <|> (Empty <$ scn)
  return $ instr sub next

parse :: Text -> Either String Term
parse s = prettify $ runParser (program <* eof) "" s
 where
  prettify (Right t  ) = Right t
  prettify (Left  err) = Left $ errorBundlePretty err
