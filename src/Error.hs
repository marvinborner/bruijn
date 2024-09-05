-- MIT License, Copyright (c) 2022 Marvin Borner
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Error where

import           Data.List                      ( intercalate )
import           Text.Megaparsec

import           Helper

errPrefix :: String
errPrefix = "\ESC[101m\ESC[30mERROR\ESC[0m "

okPrefix :: String
okPrefix = "\ESC[102m\ESC[30m OK \ESC[0m "

data Error = SyntaxError String
           | UndefinedIdentifier Identifier
           | UnmatchedMixfix [MixfixIdentifierKind] [Mixfix]
           | InvalidIndex Int
           | FailedTest Expression Expression Expression Expression
           | PassedTest Expression Expression
           | ContextualError Error Context
           | SuggestSolution Error String
           | ImportError String
           | OptimizerError String

instance Show Error where
  show (ContextualError err ctx) = show err <> "\n" <> printContext ctx
  show (SuggestSolution err sol) =
    show err <> "\n\ESC[102m\ESC[30msuggestion\ESC[0m Perhaps you meant " <> sol
  show (SyntaxError err) =
    errPrefix <> "invalid syntax\n\ESC[105m\ESC[30mnear\ESC[0m " <> err
  show (UndefinedIdentifier ident) =
    errPrefix <> "undefined identifier " <> show ident
  show (UnmatchedMixfix ks ms) =
    errPrefix
      <> "couldn't find matching mixfix for "
      <> intercalate "" (map show ks)
      <> "\n\ESC[105m\ESC[30mnear\ESC[0m "
      <> unwords (map show ms)
  show (InvalidIndex err) = errPrefix <> "invalid index " <> show err
  show (PassedTest exp1 exp2) =
    okPrefix <> "test passed: " <> show exp1 <> " = " <> show exp2
  show (FailedTest exp1 exp2 red1 red2) =
    errPrefix
      <> "test failed: "
      <> show exp1
      <> " = "
      <> show exp2
      <> "\n      reduced to "
      <> show red1
      <> " = "
      <> show red2
  show (ImportError    path) = errPrefix <> "invalid import " <> show path
  show (OptimizerError msg ) = errPrefix <> "optimizer failed: " <> msg

type Failable = Either Error

-- Modified from megaparsec's errorBundlePretty
printBundle
  :: forall s e
   . (VisualStream s, TraversableStream s, ShowErrorComponent e)
  => ParseErrorBundle s e
  -> String
printBundle ParseErrorBundle {..} =
  let (r, _) = foldl f (id, bundlePosState) bundleErrors in drop 1 (r "")
 where
  f :: (ShowS, PosState s) -> ParseError s e -> (ShowS, PosState s)
  f (o, !pst) e = (o . (outChunk ++), pst')
   where
    (msline, pst') = reachOffset (errorOffset e) pst
    epos           = pstateSourcePos pst'
    outChunk       = "\n\n" <> offendingLine <> init (parseErrorTextPretty e)
    offendingLine  = case msline of
      Nothing -> ""
      Just sline ->
        let pointer    = "^"
            rpadding   = replicate rpshift ' '
            rpshift    = unPos (sourceColumn epos) - 2
            lineNumber = (show . unPos . sourceLine) epos
            padding    = replicate (length lineNumber + 1) ' '
        in  padding
              <> "|\n"
              <> "  | "
              <> sline
              <> "\n"
              <> padding
              <> "|  "
              <> rpadding
              <> pointer
              <> "\n"
