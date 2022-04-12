module Parser where

import           Data.Functor.Identity
import           Text.Parsec
import           Text.Parsec.Language
import qualified Text.Parsec.Token             as Token

data Error = SyntaxError ParseError | UndeclaredFunction String | InvalidIndex Int | FatalError String
instance Show Error where
  show (SyntaxError        err) = show err
  show (UndeclaredFunction err) = "ERROR: undeclared function " <> show err
  show (InvalidIndex       err) = "ERROR: invalid index " <> show err
  show (FatalError         err) = show err
type Failable = Either Error

languageDef :: GenLanguageDef String u Identity
languageDef = emptyDef { Token.commentLine     = "#"
                       , Token.identStart      = letter
                       , Token.identLetter     = alphaNum <|> char '?'
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

data Expression = Bruijn Int | Variable String | Abstraction Expression | Application Expression Expression
  deriving (Ord, Eq, Show)
data Instruction = Define String Expression | Evaluate Expression | Comment String
  deriving (Show)

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
  parseAbstraction <|> parens parseApplication <|> parseBruijn <|> parseVariable

parseExpression :: Parser Expression
parseExpression = do
  expr <- parseApplication <|> parseSingleton
  pure expr

parseDefine :: Parser Instruction
parseDefine = do
  var <- identifier
  spaces
  Define var <$> parseExpression

parseComment :: Parser Instruction
parseComment = string "#" >> Comment <$> many letter

parseLine :: Parser Instruction
parseLine = try parseDefine <|> parseComment

parseReplLine :: Parser Expression
parseReplLine = try parseExpression
