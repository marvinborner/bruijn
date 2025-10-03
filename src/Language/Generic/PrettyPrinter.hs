-- MIT License, Copyright (c) 2025 Marvin Borner

module Language.Generic.PrettyPrinter (
  hover,
  text,
) where

import Data.Text (Text)
import qualified Data.Text as T
import Text.PrettyPrint.Leijen hiding (text)
import qualified Text.PrettyPrint.Leijen as PP

-- TODO: add colors etc

text :: Text -> Doc
text = PP.text . T.unpack

hover :: Text -> Doc -> Doc
hover note doc =
  string "\ESC]8;;"
    <> text note
    <> string "\ESC\\"
    <> doc
    <> string
      "\ESC]8;;\ESC\\"
