module Parser
  ( parseBlock
  , parseReplLine
  ) where

import           Control.Monad                  ( ap
                                                , void
                                                )
import           Data.Void
import           Helper
import           Text.Megaparsec         hiding ( parseTest )
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

type Parser = Parsec Void String

-- exactly one space
-- TODO: replace many scs with sc
-- sc :: Parser ()
-- sc = void $ char ' '

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

importPath :: Parser String
importPath = some $ oneOf "./_+-" <|> letterChar <|> digitChar

parseAbstraction :: Parser Expression
parseAbstraction = do
  _ <- symbol "[" <?> "opening abstraction"
  e <- parseExpression
  _ <- symbol "]" <?> "closing abstraction"
  pure $ Abstraction e

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
  e <- parseApplication <|> parseSingleton
  scs
  pure e <?> "expression"

parseEvaluate :: Parser Instruction
parseEvaluate = Evaluate <$> parseExpression

parseDefine :: Int -> Parser Instruction
parseDefine lvl = do
  inp <- getInput
  var <- defIdentifier
  scs
  e    <- parseExpression
  -- TODO: Fix >1 sub-defs
  subs <-
    (try $ newline *> (many (parseBlock (lvl + 1)))) <|> (try eof >> return [])
  pure $ Define var e subs inp

parseReplDefine :: Parser Instruction
parseReplDefine = do
  inp <- getInput
  var <- defIdentifier
  scs
  _ <- symbol "="
  scs
  e <- parseExpression
  pure $ Define var e [] inp

parseComment :: Parser ()
parseComment = do
  _ <- string "# " <?> "comment"
  _ <- some $ noneOf "\r\n"
  return ()

parseImport :: Parser Instruction
parseImport = do
  _ <- string ":import " <?> "import"
  scs
  path <- importPath
  scs
  ns <- namespace
  scs
  pure $ Import (path ++ ".bruijn") ns

parsePrint :: Parser Instruction
parsePrint = do
  _ <- string ":print " <?> "print"
  scs
  e <- parseExpression
  scs
  pure $ Evaluate e

parseTest :: Parser Instruction
parseTest = do
  _  <- string ":test " <?> "test"
  e1 <- parseExpression
  scs
  _ <- symbol "="
  scs
  e2 <- parseExpression
  pure $ Test e1 e2

parseCommentBlock :: Parser Instruction
parseCommentBlock = do
  _ <- sepEndBy1 parseComment newline
  eof
  return Comment

-- TODO: Add comment/test [Instruction] parser and combine with (this) def block?
parseDefBlock :: Int -> Parser Instruction
parseDefBlock lvl =
  (sepEndBy parseComment newline)
    *> string (replicate lvl '\t')
    *> (   try (parseDefine lvl)
       <|> try parsePrint
       <|> try parseImport
       <|> try parseTest
       )

parseBlock :: Int -> Parser Instruction
parseBlock lvl = try parseCommentBlock <|> parseDefBlock lvl

parseReplLine :: Parser Instruction
parseReplLine =
  try parseReplDefine
    <|> try parseEvaluate
    <|> try parseImport
    <|> try parseTest
