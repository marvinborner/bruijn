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

-- "'" can't be in special chars because of 'c' char notation and prefixation
specialChar :: Parser Char
specialChar = oneOf "!?*@.:;+-_#$%^&<>/\\|~="

-- lower or upper
greekLetter :: Parser Char
greekLetter = satisfy isGreek
  where isGreek c = ('Α' <= c && c <= 'Ω') || ('α' <= c && c <= 'ω')

infixOperator :: Parser Identifier
infixOperator = normalInfix <|> namespacedInfix
 where
  normalInfix     = InfixFunction <$> some specialChar
  namespacedInfix = NamespacedFunction <$> dottedNamespace <*> infixOperator

prefixOperator :: Parser Identifier
prefixOperator = normalPrefix <|> namespacedPrefix
 where
  normalPrefix     = PrefixFunction <$> some specialChar
  namespacedPrefix = NamespacedFunction <$> dottedNamespace <*> prefixOperator

defIdentifier :: Parser Identifier
defIdentifier =
  (   NormalFunction
    <$> ((:) <$> (lowerChar <|> greekLetter) <*> many
          (alphaNumChar <|> specialChar <|> char '\'')
        )
    )
    <|> (char '(' *> infixOperator <* char ')')
    <|> (prefixOperator <* char '(')
    <?> "defining identifier"

identifier :: Parser Identifier
identifier =
  try (NamespacedFunction <$> dottedNamespace <*> defIdentifier)
    <|> defIdentifier
    <?> "identifier"

namespace :: Parser String
namespace = (:) <$> upperChar <*> many letterChar <?> "namespace"

dottedNamespace :: Parser String
dottedNamespace = (\n d -> n ++ [d]) <$> namespace <*> char '.'

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
    between (char '\'')
            (char '\'')
            ((char '\\' *> specialEscape) <|> satisfy (`notElem` "\"\\"))
      <?> "quoted char"
  pure $ charToExpression ch

parseFunction :: Parser Expression
parseFunction = do
  var <- identifier
  pure $ Function var

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
    <|> try parseFunction
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

parseImport :: Parser Command
parseImport = do
  _    <- string ":import " <?> "import instruction"
  path <- importPath
  ns   <- (try $ (sc *> (namespace <|> string "."))) <|> (eof >> return "")
  pure (Import (path ++ ".bruijn") ns)

parseInput :: Parser Command
parseInput = do
  _    <- string ":input " <?> "input instruction"
  path <- importPath
  pure (Input $ path ++ ".bruijn")

parseTest :: Parser Command
parseTest = do
  _  <- string ":test " <?> "test"
  e1 <- (parens parseExpression <?> "first expression")
  sc
  e2 <- (parens parseExpression <?> "second expression")
  pure (Test e1 e2)

parseCommentBlock :: Parser Instruction
parseCommentBlock = do
  inp <- getInput
  _   <- sepEndBy1 parseComment newline
  eof
  return $ ContextualInstruction Comment inp

parseCommandBlock :: Parser Instruction
parseCommandBlock = do
  inp      <- getInput
  commands <-
    sepEndBy1 parseTest newline
    <|> sepEndBy1 parseInput  newline
    <|> sepEndBy1 parseImport newline
  return $ ContextualInstruction (Commands commands) inp

parseDefBlock :: Int -> Parser Instruction
parseDefBlock lvl =
  (sepEndBy parseComment newline)
    *> string (replicate lvl '\t')
    *> (try (parseDefine lvl))

parseBlock :: Int -> Parser Instruction
parseBlock lvl =
  try parseCommentBlock <|> try (parseDefBlock lvl) <|> parseCommandBlock

parseReplLine :: Parser Instruction
parseReplLine =
  try parseReplDefine -- TODO: This is kinda hacky
    <|> ((Commands . (: [])) <$> (try parseImport))
    <|> ((Commands . (: [])) <$> (try parseTest))
    <|> try parseEvaluate
