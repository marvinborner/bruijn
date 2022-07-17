module Parser
  ( parseLine
  , parseReplLine
  ) where

import           Control.Monad                  ( ap )
import           Data.Functor.Identity
import           Data.Void
import           Helper
import           Text.Megaparsec         hiding ( parseTest )
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

identifier :: Parser String
identifier = lexeme
  ((:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> oneOf "?!'_-"))

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

almostAnything :: Parser String
almostAnything =
  some $ oneOf ".`#~@$%^&*_+-=|;',/?[]<>(){} " <|> letterChar <|> digitChar

importPath :: Parser String
importPath = some $ oneOf "./_+-" <|> letterChar <|> digitChar

parseAbstraction :: Parser Expression
parseAbstraction = do
  symbol "["
  exp <- parseExpression
  symbol "]"
  pure $ Abstraction exp

parseApplication :: Parser Expression
parseApplication = do
  s <- sepBy1 parseSingleton space
  pure $ foldl1 Application s

parseBruijn :: Parser Expression
parseBruijn = do
  idx <- digitChar
  space
  pure $ Bruijn $ (read . pure) idx

parseNumeral :: Parser Expression
parseNumeral = do
  num <- number
  space
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
  space
  pure $ Variable var

parseSingleton :: Parser Expression
parseSingleton =
  parseBruijn
    <|> parseNumeral
    <|> parseAbstraction
    <|> parens parseApplication
    <|> parseVariable

parseExpression :: Parser Expression
parseExpression = do
  space
  expr <- parseApplication <|> parseSingleton
  space
  pure expr

parseEvaluate :: Parser Instruction
parseEvaluate = Evaluate <$> parseExpression

parseDefine :: Parser Instruction
parseDefine = do
  var <- identifier
  space
  Define var <$> parseExpression

parseReplDefine :: Parser Instruction
parseReplDefine = do
  var <- identifier
  space
  symbol "="
  space
  Define var <$> parseExpression

parseComment :: Parser Instruction
parseComment = string "#" >> Comment <$> almostAnything

parseImport :: Parser Instruction
parseImport = do
  string ":import "
  space
  path <- importPath
  space
  pure $ Import $ path ++ ".bruijn"

parsePrint :: Parser Instruction
parsePrint = do
  string ":print "
  space
  exp <- parseExpression
  space
  pure $ Evaluate exp

parseTest :: Parser Instruction
parseTest = do
  string ":test "
  exp1 <- parseExpression
  space
  symbol "="
  space
  exp2 <- parseExpression
  pure $ Test exp1 exp2

parseLine :: Parser Instruction
parseLine =
  try parseDefine
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
