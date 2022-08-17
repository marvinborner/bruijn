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

specialChar :: Parser Char
specialChar = oneOf "!?*@.:;+-_#$%^&<>/\\|~="

infixOperator :: Parser String
infixOperator = some specialChar

prefixOperator :: Parser String
prefixOperator = some specialChar

-- def identifier disallows the import prefix dots
defIdentifier :: Parser String
defIdentifier =
  ((:) <$> letterChar <*> many (alphaNumChar <|> specialChar <|> char '\''))
    <|> ((\l i r -> [l] ++ i ++ [r]) <$> char '(' <*> infixOperator <*> char ')'
        )
    <|> ((\p i -> p ++ [i]) <$> prefixOperator <*> char '(')
    <?> "defining identifier"

-- TODO: write as extension to defIdentifier
identifier :: Parser String
identifier =
  ((:) <$> letterChar <*> many (alphaNumChar <|> specialChar <|> oneOf ".\'"))
    <|> ((\l i r -> [l] ++ i ++ [r]) <$> char '(' <*> infixOperator <*> char ')'
        )
    <|> ((\p i -> p ++ [i]) <$> prefixOperator <*> char '(')
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
  s <- sepEndBy1 (try parsePrefix <|> parseSingleton) sc
  pure $ foldl1 Application s

parseBruijn :: Parser Expression
parseBruijn = do
  idx <- digitChar <?> "bruijn index"
  pure $ Bruijn $ (read . pure) idx

parseNumeral :: Parser Expression
parseNumeral = do
  num <- parens number <?> "signed number"
  pure $ decimalToTernary num
 where
  sign :: Parser (Integer -> Integer)
  sign = (char '-' >> return negate) <|> (char '+' >> return id)
  nat :: Parser Integer
  nat = read <$> some digitChar
  number :: Parser Integer
  number = ap sign nat

specialEscape :: Parser Char
specialEscape =
  choice (zipWith (\c r -> r <$ char c) "bnfrt\\\"/" "\b\n\f\r\t\\\"/")

parseString :: Parser Expression
parseString = do
  str <-
    between
        (char '\"')
        (char '\"')
        (some $ (char '\\' *> specialEscape) <|> (satisfy (`notElem` "\"\\")))
      <?> "quoted string"
  pure $ stringToExpression str

parseChar :: Parser Expression
parseChar = do
  ch <-
    between (char '\'') (char '\'') (satisfy (`notElem` "\"\\"))
      <?> "quoted char"
  pure $ charToExpression ch

parseVariable :: Parser Expression
parseVariable = do
  var <- identifier
  pure $ Variable var

parseInfix :: Parser Expression
parseInfix = do
  e1 <- parseSingleton
  sc
  i <- infixOperator
  sc
  e2 <- parseSingleton
  pure $ Infix e1 i e2

parsePrefix :: Parser Expression
parsePrefix = do
  p <- prefixOperator
  e <- parseSingleton
  pure $ Prefix p e

parseSingleton :: Parser Expression
parseSingleton =
  parseBruijn
    <|> try parseNumeral
    <|> parseString
    <|> parseChar
    <|> parseAbstraction
    <|> try parseVariable
    <|> try (parens parseInfix <?> "enclosed infix expr")
    <|> (parens parseApplication <?> "enclosed application")
    <|> parsePrefix

parseExpression :: Parser Expression
parseExpression = do
  e <- try parseInfix <|> try parseApplication <|> parsePrefix
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
  _    <- string ":import " <?> "import instruction"
  path <- importPath
  ns   <- (try $ sc *> namespace) <|> (eof >> return "")
  pure $ ContextualInstruction (Import (path ++ ".bruijn") ns) inp

parseInput :: Parser Instruction
parseInput = do
  inp  <- getInput
  _    <- string ":input " <?> "input instruction"
  path <- importPath
  pure $ ContextualInstruction (Input $ path ++ ".bruijn") inp

parsePrint :: Parser Instruction
parsePrint = do
  inp <- getInput
  _   <- string ":print " <?> "print instruction"
  e   <- parseExpression
  pure $ ContextualInstruction (Evaluate e) inp

parseTest :: Parser Instruction
parseTest = do
  inp <- getInput
  _   <- string ":test " <?> "test"
  e1  <- (parens parseExpression <?> "first expression")
  sc
  e2 <- (parens parseExpression <?> "second expression")
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
       <|> try parseInput
       <|> try parseTest
       )

parseBlock :: Int -> Parser Instruction
parseBlock lvl = try parseCommentBlock <|> parseDefBlock lvl

parseReplLine :: Parser Instruction
parseReplLine =
  try parseReplDefine
    <|> try parseImport
    <|> try parseTest
    <|> try parseEvaluate
