module Parser where

import           Data.Functor.Identity
import           Helper
import           Text.Parsec
import           Text.Parsec.Language
import qualified Text.Parsec.Token             as Token

languageDef :: GenLanguageDef String u Identity
languageDef = emptyDef { Token.commentLine     = "#"
                       , Token.identStart      = letter
                       , Token.identLetter     = alphaNum <|> char '?'
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

parseVariable :: Parser Expression
parseVariable = do
  var <- identifier
  spaces
  pure $ Variable var

parseSingleton :: Parser Expression
parseSingleton =
  parseBruijn <|> parseAbstraction <|> parens parseApplication <|> parseVariable

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

parseLoad :: Parser Instruction
parseLoad = string ":load " >> Load <$> almostAnything

parseLine :: Parser Instruction
parseLine = try parseDefine <|> parseComment

parseReplLine :: Parser Instruction
parseReplLine =
  try parseReplDefine <|> parseComment <|> parseEvaluate <|> parseLoad
