module Parser where

import           Data.Functor.Identity
import           Text.Parsec
import           Text.Parsec.Language
import qualified Text.Parsec.Token             as Token

languageDef :: GenLanguageDef String u Identity
languageDef = emptyDef { Token.commentLine     = "#"
                       , Token.identStart      = letter
                       , Token.identLetter     = alphaNum <|> char '_'
                       , Token.reservedOpNames = ["[", "]"]
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

data Expression = Index Int | Abstraction Int Expression | Application Expression Expression
  deriving (Ord, Eq)
data Instruction = Define String Expression | Evaluate Expression | Comment String

parseAbstraction :: Parser Expression
parseAbstraction = do
  reservedOp "["
  idc <- endBy1 digit spaces
  build idc <$> parseExpression
 where
  build (idx : idc) body =
    Abstraction ((read . pure :: Char -> Int) idx) $ build idc body
  curry [] body = body

parseApplication :: Parser Expression
parseApplication = do
  s <- sepBy1 parseSingleton spaces
  pure $ foldl1 Application s

parseSingleton :: Parser Expression
parseSingleton = parseAbstraction <|> parens parseApplication

parseExpression :: Parser Expression
parseExpression = do
  expr <- parseApplication <|> parseSingleton
  pure expr

parseDefine :: Parser Instruction
parseDefine = do
  var <- identifier
  space
  Define var <$> parseExpression

parseLine :: Parser Instruction
parseLine = try parseDefine

parseReplLine :: Parser Expression
parseReplLine = try parseExpression
