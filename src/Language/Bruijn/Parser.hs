-- MIT License, Copyright (c) 2025 Marvin Borner

module Language.Bruijn.Parser (
  parse,
) where

import Control.Monad (ap, void)
import Control.Monad.State (
  State,
  evalState,
  modify,
 )
import Data.Bruijn (
  -- FloatingEncoding (..),
  Identifier (..),
  IntegerEncoding (..),
  MixfixIdentifier (..),
  Name,
  SyntacticSugar (..),
  TermAnn,
  TermF (..),
 )
import Data.Context (
  Context (..),
  MetaLevel (..),
 )
import Data.Foreign (ForeignLanguage (..))
import Data.Functor (($>))
import Data.Phase (Phase (BruijnParse))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Language.Generic.Annotation (
  annotate,
  fakeSrcSpan,
 )
import Language.Generic.Error (
  Error (..),
  MonadError,
  throwError,
 )
import Text.Megaparsec hiding (
  State,
  parse,
 )
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = ParsecT Void Text (State (Context BruijnParse))
type PhaseError = MonadError (Error BruijnParse)
type Term = TermAnn BruijnParse

-- | sync position in State
syncPos :: Parser a -> Parser a
syncPos p = do
  pos <- getSourcePos
  modify $ \ctx -> ctx{_srcPos = pos}
  p

greekLetter :: Parser Char
greekLetter = satisfy isGreek
 where
  isGreek c = ('Α' <= c && c <= 'Ω') || ('α' <= c && c <= 'ω')

emoticon :: Parser Char
emoticon = satisfy isEmoticon
 where
  isEmoticon c = '\128512' <= c && c <= '\128591'

mathematicalOperator :: Parser Char
mathematicalOperator =
  satisfy isMathematicalUnicodeBlock
    <|> satisfy isMiscMathematicalAUnicodeBlock
    <|> oneOf ("¬₀₁₂₃₄₅₆₇₈₉₊₋₌₍₎⁰¹²³⁴⁵⁶⁷⁸⁹ᵃᵇᶜᵈᵉᶠᵍʰʲᵏˡᵐᵒᵖʳˢᵗᵘᵛʷˣʸᶻ⁺⁻⁼⁽⁾" :: String)
 where
  isMathematicalUnicodeBlock c = '∀' <= c && c <= '⋿'
  isMiscMathematicalAUnicodeBlock c = '⟀' <= c && c <= '⟯'

mathematicalArrow :: Parser Char
mathematicalArrow = satisfy isMathematicalOperator
 where
  isMathematicalOperator c = '←' <= c && c <= '⇿'

generalPunctuation :: Parser Char
generalPunctuation = satisfy isGeneralPunctuation
 where
  isGeneralPunctuation c = '‐' <= c && c <= '⁞' && c /= '…' && c /= '‣'

shapes :: Parser Char
shapes = satisfy isShapes where isShapes c = '─' <= c && c <= '◿'

specialChar :: Parser Char
specialChar =
  oneOf ("!?*@:;+-_#$%^&<>/\\|{}~=" :: String)
    <|> mathematicalOperator
    <|> mathematicalArrow
    <|> generalPunctuation
    <|> shapes

startChar :: Parser Char
startChar = lowerChar <|> greekLetter <|> emoticon

restChar :: Parser Char
restChar = alphaNumChar <|> specialChar <|> char '\''

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
sc =
  syncPos $
    L.space (void $ some (oneOf (" " :: String))) lineComment blockComment

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
special = T.pack <$> some specialChar

local :: Parser Identifier
local = Local . T.pack <$> ((:) <$> startChar <*> many restChar)

namespaced :: Parser Identifier
namespaced = Namespaced <$> namespace <*> identifier

mixfix :: Parser Identifier
mixfix = Mixfix <$> some mixfixIdentifier
 where
  mixfixIdentifier = (Wildcard <$ char '…') <|> (Special <$> special)

prefix :: Parser Identifier
prefix = Prefix <$> special <* char '‣' <?> "prefix"

identifier :: Parser Identifier
identifier = local <|> namespaced <|> try prefix <|> mixfix <?> "identifier"

abstraction :: Parser Term
abstraction =
  annotate $ AbstractionF <$> (symbolN "[" *> lexeme lambda <* scn <* symbol "]")

application :: Parser Term
application =
  annotate $
    ApplicationF <$> (symbolN "(" *> some (lexeme singleton) <* scn <* symbol ")")

index :: Parser Term
index = annotate $ IndexF . read . return <$> digitChar

force :: Parser Term
force = annotate $ ForceF <$ char '!'

substitution :: Parser Term
substitution = annotate $ SubstitutionF <$> lexeme identifier

-- stringSugar =

number :: Parser Integer
number = ap sign nat
 where
  nat = read <$> some digitChar
  sign = (char '-' $> negate) <|> (char '+' $> id)

integerSugar :: Parser SyntacticSugar
integerSugar =
  symbol "("
    *> ( flip IntegerNumber
           <$> number
           <*> ( (char 'u' $> Unary)
                   <|> (char 'b' $> Binary)
                   <|> (char 't' $> BalancedTernary)
                   <|> (char 'd' $> DeBruijn)
               )
       )
    <* symbol ")"

sugar :: Parser Term
sugar = annotate $ SugarF <$> integerSugar -- stringSugar <|> integerSugar <|> floatingSugar

singleton :: Parser Term
singleton =
  lexeme $
    try sugar <|> abstraction <|> application <|> index <|> force <|> substitution

lambda :: Parser Term
lambda = annotate $ ApplicationF <$> some (lexeme singleton)

-- termType :: Parser Term
-- termType =

definition :: Parser (Term -> Term -> TermF Term)
definition = DefinitionF <$> lexeme identifier <*> lexeme lambda --  <*> optional (symbol "⧗" *> signature)

freestanding :: Parser (Term -> Term -> TermF Term)
freestanding = DoF <$> (symbol "do " *> lexeme lambda)

command :: Parser Term
command = annotate $ char ':' *> (try test <|> imp)
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
program = annotate $ do
  indent <- scn *> L.indentLevel
  instr <- freestanding <|> try preprocessor <|> definition
  sub <- try (L.indentGuard scn GT indent *> program) <|> annotate (EmptyF <$ scn)
  next <-
    try (L.indentGuard scn EQ indent *> program) <|> annotate (EmptyF <$ scn)
  return $ instr sub next

parse :: (PhaseError m) => Text -> Text -> m Term
parse file input =
  prettify $
    evalState
      (runParserT (program <* eof) (T.unpack file) input)
      initialContext
 where
  initialContext =
    ( Context
        { _lang = Bruijn
        , _srcPos = initialPos $ T.unpack file
        , _srcSpan = fakeSrcSpan
        , _metaLevel = MetaLevel 0
        }
    )
  prettify (Right t) = return t
  prettify (Left err) =
    throwError $ Error initialContext $ T.pack $ errorBundlePretty err
