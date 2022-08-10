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

type Parser = Parsec Void String

-- exactly one space
sc :: Parser ()
sc = void $ char ' '

-- zero or more spaces
-- scs :: Parser ()
-- scs = void $ takeWhileP (Just "white space") (== ' ')

-- def identifier disallows the import prefix dots
defIdentifier :: Parser String
defIdentifier =
  ((:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> oneOf "?!'_-"))
    <?> "defining identifier"

-- TODO: write as extension to defIdentifier
identifier :: Parser String
identifier =
  ((:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> oneOf "?!'_-."))
    <?> "identifier"

namespace :: Parser String
namespace =
  ((:) <$> upperChar <*> many letterChar) <|> string "." <?> "namespace"

parens :: Parser a -> Parser a
parens = between (string "(") (string ")")

importPath :: Parser String
importPath = some $ oneOf "./_+-" <|> letterChar <|> digitChar

parseAbstraction :: Parser Expression
parseAbstraction = do
  _ <- string "[" <?> "opening abstraction"
  e <- parseExpression
  _ <- string "]" <?> "closing abstraction"
  pure $ Abstraction e

-- one or more singletons wrapped in coupled application
parseApplication :: Parser Expression
parseApplication = do
  s <- sepEndBy1 parseSingleton sc -- TODO: Fix consuming space at end (re. test =)
  pure $ foldl1 Application s

parseBruijn :: Parser Expression
parseBruijn = do
  idx <- digitChar <?> "bruijn index"
  pure $ Bruijn $ (read . pure) idx

parseNumeral :: Parser Expression
parseNumeral = do
  num <- number <?> "signed number"
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
  e <- parseApplication
  pure e <?> "expression"

parseEvaluate :: Parser Instruction
parseEvaluate = do
  inp <- getInput
  e   <- parseExpression
  pure $ ContextualInstruction (Evaluate e) inp

parseDefine :: Int -> Parser Instruction
parseDefine lvl = do
  inp <- getInput
  var <- defIdentifier
  sc
  e    <- parseExpression
  -- TODO: Fix >1 sub-defs
  subs <-
    (try $ newline *> (many (parseBlock (lvl + 1)))) <|> (try eof >> return [])
  pure $ ContextualInstruction (Define var e subs) inp

parseReplDefine :: Parser Instruction
parseReplDefine = do
  inp <- getInput
  var <- defIdentifier
  _   <- string " = "
  e   <- parseExpression
  pure $ ContextualInstruction (Define var e []) inp

parseComment :: Parser ()
parseComment = do
  _ <- string "# " <?> "comment"
  _ <- some $ noneOf "\r\n"
  return ()

parseImport :: Parser Instruction
parseImport = do
  inp  <- getInput
  _    <- string ":import " <?> "import"
  path <- importPath
  ns   <- (try $ sc *> namespace) <|> (eof >> return "")
  pure $ ContextualInstruction (Import (path ++ ".bruijn") ns) inp

parsePrint :: Parser Instruction
parsePrint = do
  inp <- getInput
  _   <- string ":print " <?> "print"
  e   <- parseExpression
  pure $ ContextualInstruction (Evaluate e) inp

parseTest :: Parser Instruction
parseTest = do
  inp <- getInput
  _   <- string ":test " <?> "test"
  e1  <- parseExpression
  _   <- string "= " -- TODO: Disallow missing space (non-trivial)
  e2  <- parseExpression
  pure $ ContextualInstruction (Test e1 e2) inp

parseCommentBlock :: Parser Instruction
parseCommentBlock = do
  inp <- getInput
  _   <- sepEndBy1 parseComment newline
  eof
  return $ ContextualInstruction Comment inp

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
