module Parser
  ( parseLine
  , parseReplLine
  ) where

import           Control.Monad                  ( ap )
import           Data.Functor.Identity
import           Helper
import           Text.Parsec             hiding ( parseTest )
import           Text.Parsec.Language
import qualified Text.Parsec.Token             as Token

languageDef :: GenLanguageDef String u Identity
languageDef = emptyDef { Token.commentLine     = "#"
                       , Token.identStart      = letter <|> char '_'
                       , Token.identLetter     = alphaNum <|> oneOf "?!'_"
                       , Token.reservedOpNames = ["[", "]", "="]
                       }

type Parser = Parsec String ()

lexer :: Token.GenTokenParser String u Identity
lexer = Token.makeTokenParser languageDef

identifier :: Parser String
identifier = Token.identifier lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

almostAnything :: Parser String
almostAnything =
  many1 $ oneOf ".`#~@$%^&*_+-=|;',/?[]<>(){} " <|> letter <|> digit

importPath :: Parser String
importPath = many1 $ oneOf "./_+-" <|> letter <|> digit

parseAbstraction :: Parser Expression
parseAbstraction = do
  reservedOp "["
  exp <- parseExpression
  reservedOp "]"
  pure $ Abstraction exp

parseApplication :: Parser Expression
parseApplication = do
  s <- sepBy1 parseSingleton spaces
  pure $ foldl1 Application s

parseBruijn :: Parser Expression
parseBruijn = do
  idx <- digit
  spaces
  pure $ Bruijn $ (read . pure) idx

parseNumeral :: Parser Expression
parseNumeral = do
  num <- number
  spaces
  pure $ decimalToBinary num
 where
  sign   = (char '-' >> return negate) <|> (char '+' >> return id)
  nat    = read <$> many1 digit
  number = ap sign nat

parseVariable :: Parser Expression
parseVariable = do
  var <- identifier
  spaces
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
  spaces
  expr <- parseApplication <|> parseSingleton
  spaces
  pure expr

parseEvaluate :: Parser Instruction
parseEvaluate = Evaluate <$> parseExpression

parseDefine :: Parser Instruction
parseDefine = do
  var <- identifier
  spaces
  Define var <$> parseExpression

parseReplDefine :: Parser Instruction
parseReplDefine = do
  var <- identifier
  spaces
  reservedOp "="
  spaces
  Define var <$> parseExpression

parseComment :: Parser Instruction
parseComment = string "#" >> Comment <$> almostAnything

parseImport :: Parser Instruction
parseImport = do
  string ":import "
  spaces
  path <- importPath
  spaces
  pure $ Import $ path ++ ".bruijn"

parsePrint :: Parser Instruction
parsePrint = do
  string ":print "
  spaces
  exp <- parseExpression
  spaces
  pure $ Evaluate exp

parseTest :: Parser Instruction
parseTest = do
  string ":test "
  exp1 <- parseExpression
  spaces
  reservedOp "="
  spaces
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
