-- MIT License, Copyright (c) 2025 Marvin Borner

module Language.Bruijn.Parser
  ( parse
  ) where

import           Control.Monad                  ( void )
import           Control.Monad.State            ( State
                                                , evalState
                                                , modify
                                                , put
                                                )
import           Data.Bruijn                    ( Identifier(..)
                                                , MixfixIdentifier(..)
                                                , Name
                                                , SyntacticSugar(..)
                                                , TermAnn(..)
                                                , TermF(..)
                                                )
import           Data.Context                   ( Context(..)
                                                , MetaLevel(..)
                                                )
import           Data.Fix                       ( Fix(..) )
import           Data.Foreign                   ( ForeignLanguage(..) )
import           Data.Phase                     ( Phase(BruijnParse) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Void
import           Language.Generic.Annotation    ( ann
                                                , fakeSrcSpan
                                                )
import           Language.Generic.Error         ( Error(..)
                                                , MonadError
                                                , PhaseT
                                                , liftPhase
                                                , throwError
                                                )
import           Text.Megaparsec         hiding ( State
                                                , parse
                                                )
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

type Parser = ParsecT Void Text (State (Context BruijnParse))
type PhaseError = MonadError (Error BruijnParse)
type Term = TermAnn BruijnParse

-- | sync position in State
syncPos :: Parser a -> Parser a
syncPos p = do
  pos <- getSourcePos
  modify $ \ctx -> ctx { srcPos = pos }
  p

-- | single line comment
lineComment :: Parser ()
lineComment = L.skipLineComment "# "

-- | multiline comment
blockComment :: Parser ()
blockComment = L.skipBlockComment "###" "###"

-- | space consumer including comments and newlines
scn :: Parser ()
scn = syncPos $ L.space space1 lineComment blockComment

-- | space consumer including comments without newlines
sc :: Parser ()
sc = syncPos
  $ L.space (void $ some (oneOf (" \t" :: String))) lineComment blockComment

-- | lexeme consumer without newline
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | symbol consumer without newline
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | symbol consumer with newline
symbolN :: Text -> Parser Text
symbolN = L.symbol scn

path :: Parser Text
path = T.pack <$> some alphaNumChar

namespace :: Parser Name
namespace = T.pack <$> ((:) <$> upperChar <*> many letterChar)

special :: Parser Name
special = T.pack <$> some alphaNumChar -- TODO!

local :: Parser Identifier
local = Local . T.pack <$> some alphaNumChar

namespaced :: Parser Identifier
namespaced = Namespaced <$> namespace <*> identifier

mixfix :: Parser Identifier
mixfix = Mixfix <$> some mixfixIdentifier
  where mixfixIdentifier = (Wildcard <$ char '…') <|> (Special <$> special)

identifier :: Parser Identifier
identifier = local <|> namespaced <|> mixfix

abstraction :: Parser Term
abstraction =
  ann $ AbstractionF <$> (symbol "[" *> lexeme lambda <* symbol "]")

application :: Parser Term
application =
  ann $ ApplicationF <$> (symbol "(" *> some (lexeme singleton) <* symbol ")")

index :: Parser Term
index = ann $ IndexF . read . return <$> digitChar

force :: Parser Term
force = ann $ ForceF <$ char '!'

substitution :: Parser Term
substitution = ann $ SubstitutionF <$> lexeme identifier

singleton :: Parser Term
singleton =
  lexeme $ abstraction <|> application <|> index <|> force <|> substitution

lambda :: Parser Term
lambda = ann $ ApplicationF <$> some (lexeme singleton)

definition :: Parser (Term -> Term -> TermF Term)
definition = DefinitionF <$> lexeme identifier <*> lexeme lambda

freestanding :: Parser (Term -> Term -> TermF Term)
freestanding = DoF <$> (symbol "do " *> lexeme lambda)

command :: Parser Term
command = ann $ char ':' *> (try test <|> imp)
 where
  test =
    TestF
      <$> (symbol "test" *> symbol "(" *> lexeme lambda <* symbol ")")
      <*> (symbol "(" *> lexeme lambda <* symbol ")")
  imp =
    ImportF
      <$> (symbol "import" *> lexeme path)
      <*> (lexeme namespace <|> symbol ".")

preprocessor :: Parser (Term -> Term -> TermF Term)
preprocessor = PreprocessorF <$> lexeme command

program :: Parser Term
program = ann $ do
  indent <- scn *> L.indentLevel
  instr  <- freestanding <|> try definition <|> preprocessor
  sub    <- try (L.indentGuard scn GT indent *> program) <|> ann (EmptyF <$ scn)
  next   <- try (L.indentGuard scn EQ indent *> program) <|> ann (EmptyF <$ scn)
  return $ instr sub next

parse :: PhaseError m => Text -> Text -> m Term
parse file input = prettify $ evalState
  (runParserT (program <* eof) (T.unpack file) input)
  initialContext
 where
  initialContext =
    (Context { lang      = Bruijn
             , srcPos    = initialPos $ T.unpack file
             , srcSpan   = fakeSrcSpan
             , metaLevel = MetaLevel 0
             }
    )
  prettify (Right t) = return t
  prettify (Left err) =
    throwError $ Error initialContext $ T.pack $ errorBundlePretty err
