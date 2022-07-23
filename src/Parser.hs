module Parser
  ( parseBlock
  , parseReplLine
  ) where

import           Control.Monad                  ( ap
                                                , void
                                                )
import           Data.Functor.Identity
import           Data.Void
import           Helper
import           Text.Megaparsec         hiding ( parseTest )
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

type Parser = Parsec Void String

-- exactly one space
-- TODO: replace many scs with sc
sc :: Parser ()
sc = void $ char ' '

-- zero or more spaces
scs :: Parser ()
scs = void $ takeWhileP (Just "white space") (== ' ')

lexeme :: Parser a -> Parser a
lexeme = L.lexeme scs

symbol :: String -> Parser String
symbol = L.symbol scs

-- def identifier disallows the import prefix dots
defIdentifier :: Parser String
defIdentifier =
  lexeme
      ((:) <$> (letterChar <|> char '_') <*> many
        (alphaNumChar <|> oneOf "?!'_-")
      )
    <?> "defining identifier"

-- TODO: write as extension to defIdentifier
identifier :: Parser String
identifier =
  lexeme
      ((:) <$> (letterChar <|> char '_') <*> many
        (alphaNumChar <|> oneOf "?!'_-.")
      )
    <?> "identifier"

namespace :: Parser String
namespace =
  lexeme ((:) <$> upperChar <*> many letterChar)
    <|> string "."
    <|> (scs >> return "")
    <?> "namespace"

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

almostAnything :: Parser String
almostAnything =
  some $ oneOf ".`#~@$%^&*_+-=|;',/?[]<>(){} " <|> letterChar <|> digitChar

importPath :: Parser String
importPath = some $ oneOf "./_+-" <|> letterChar <|> digitChar

parseAbstraction :: Parser Expression
parseAbstraction = do
  symbol "[" <?> "opening abstraction"
  exp <- parseExpression
  symbol "]" <?> "closing abstraction"
  pure $ Abstraction exp

parseApplication :: Parser Expression
parseApplication = do
  s <- sepBy1 parseSingleton scs
  pure $ foldl1 Application s

parseBruijn :: Parser Expression
parseBruijn = do
  idx <- digitChar
  scs
  pure $ Bruijn $ (read . pure) idx

parseNumeral :: Parser Expression
parseNumeral = do
  num <- number <?> "signed number"
  scs
  pure $ decimalToTernary num
 where
  sign :: Parser (Integer -> Integer)
  sign = (char '-' >> return negate) <|> (char '+' >> return id)
  nat :: Parser Integer
  nat = read <$> some digitChar
  number :: Parser Integer
  number = ap sign nat

parseVariable :: Parser Expression
parseVariable = do
  var <- identifier
  scs
  pure $ Variable var

parseSingleton :: Parser Expression
parseSingleton =
  parseBruijn
    <|> parseNumeral
    <|> parseAbstraction
    <|> (parens parseApplication <?> "enclosed application")
    <|> parseVariable

parseExpression :: Parser Expression
parseExpression = do
  scs
  expr <- parseApplication <|> parseSingleton
  scs
  pure expr <?> "expression"

parseEvaluate :: Parser Instruction
parseEvaluate = Evaluate <$> parseExpression

parseDefine :: Int -> Parser Instruction
parseDefine lvl = do
  var <- defIdentifier
  scs
  exp  <- parseExpression
  -- TODO: Fix >1 sub-defs
  subs <-
    (try $ newline *> (sepEndBy (parseBlock (lvl + 1)) newline))
      <|> (try eof >> return [])
  pure $ Define var exp subs

parseReplDefine :: Parser Instruction
parseReplDefine = do
  var <- defIdentifier
  scs
  symbol "="
  scs
  exp <- parseExpression
  pure $ Define var exp []

parseComment :: Parser Instruction
parseComment = string "#" >> Comment <$> almostAnything <?> "comment"

parseImport :: Parser Instruction
parseImport = do
  string ":import " <?> "import"
  scs
  path <- importPath
  scs
  ns <- namespace
  scs
  pure $ Import (path ++ ".bruijn") ns

parsePrint :: Parser Instruction
parsePrint = do
  string ":print " <?> "print"
  scs
  exp <- parseExpression
  scs
  pure $ Evaluate exp

parseTest :: Parser Instruction
parseTest = do
  string ":test " <?> "test"
  exp1 <- parseExpression
  scs
  symbol "="
  scs
  exp2 <- parseExpression
  pure $ Test exp1 exp2

-- TODO: Add comment/test [Instruction] parser and combine with (this) def block
parseBlock :: Int -> Parser Instruction
parseBlock lvl =
  string (replicate lvl '\t')
    *>  try (parseDefine lvl)
    <|> try parseComment
    <|> try parsePrint
    <|> try parseImport
    <|> try parseTest

parseReplLine :: Parser Instruction
parseReplLine =
  try parseReplDefine
    <|> try parseComment
    <|> try parseEvaluate
    <|> try parseImport
    <|> try parseTest
