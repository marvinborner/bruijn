-- MIT License, Copyright (c) 2022 Marvin Borner
module Humanification where

import           Control.Applicative            ( (<|>) )
import           Data.List                      ( intercalate )
import           Data.Maybe                     ( fromMaybe )

import           Conversion
import           Helper

-- TODO: Show list as pair if not ending with empty
maybeHumanifyExpression :: Expression -> Maybe String
maybeHumanifyExpression e =
  unaryToDecimal e
    <|> binaryToChar e
    <|> binaryToString e
    <|> ternaryToString e
    <|> rationalToString e
    <|> realToString e
    <|> complexToString e
    <|> humanifyString e
    <|> humanifyList e
    <|> humanifyPair e
    <|> humanifyMeta e

humanifyExpression :: Expression -> String
humanifyExpression e = fromMaybe "" (maybeHumanifyExpression e)

humanifyMeta :: Expression -> Maybe String
humanifyMeta e = ("`" <>) <$> go e
 where
  go (Abstraction (Abstraction (Abstraction (Application (Bruijn 0) t)))) =
    go t >>= (\a -> pure $ "[" <> a <> "]")
  go (Abstraction (Abstraction (Abstraction (Application (Application (Bruijn 1) a) b))))
    = go a >>= \l -> go b >>= \r -> pure $ "(" <> l <> " " <> r <> ")"
  go (Abstraction (Abstraction (Abstraction (Application (Bruijn 2) n)))) =
    fmap show (unaryToDecimal' n)
  go _ = Nothing

humanifyList :: Expression -> Maybe String
humanifyList e = do
  es <- unlistify e
  let conv x = fromMaybe (show x) (maybeHumanifyExpression x)
      m = map conv es
  pure $ "{" <> intercalate ", " m <> "}"

humanifyString :: Expression -> Maybe String
humanifyString e = do
  es  <- unlistify e
  str <- mapM binaryToChar' es
  pure $ "\"" <> str <> "\""

humanifyPair :: Expression -> Maybe String
humanifyPair e = do
  es <- unpairify e
  let conv x = fromMaybe (show x) (maybeHumanifyExpression x)
      m = map conv es
  pure $ "<" <> intercalate " : " m <> ">"
