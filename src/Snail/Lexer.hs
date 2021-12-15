module Snail.Lexer (
  lexeme,
  parens,
  signedInteger,
  skipBlockComment,
  skipLineComment,
  spaces,
  symbol,
  Parser,
) where

import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

-- | TODO: 'Void' is the error type but we should use an explicit error type
type Parser = Parsec Void Text

-- | Megaparsec's 'skipLineComment' takes a prefix and skips lines that begin with that prefix
skipLineComment :: Parser ()
skipLineComment = L.skipLineComment ";"

-- | Megaparsec's 'skipBlockComment' takes prefix and suffix and skips anything in between
skipBlockComment :: Parser ()
skipBlockComment = L.skipBlockCommentNested "#|" "|#"

-- | Generate a parser for whitespace in a language with 'skipLineComment' and 'skipBlockComment'
spaces :: Parser ()
spaces = L.space space1 skipLineComment skipBlockComment

-- | A 'lexeme' is ...
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaces

-- | Parse a 'Text' verbatim
symbol :: Text -> Parser Text
symbol = L.symbol spaces

-- | Parse an 'Integer' as a lexeme
integer :: Parser Integer
integer = lexeme L.decimal

-- | Parse a signed 'Integer'
signedInteger :: Parser Integer
signedInteger = L.signed spaces integer

-- | Parse an S-Expression
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
