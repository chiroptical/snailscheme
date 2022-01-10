module Snail.Lexer (
  -- * The only parser you'll ever need
  SExpression (..),
  sExpression,
) where

import Control.Monad (when)
import Data.Functor (($>))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void
import Snail.Characters
import Text.Megaparsec hiding (Tokens (..))
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

-- | TODO: 'Void' is the error type but we should use an explicit error type
type Parser = Parsec Void Text

{- | Megaparsec's 'skipLineComment' takes a prefix and skips lines that begin
 with that prefix
-}
skipLineComment :: Parser ()
skipLineComment = L.skipLineComment ";"

{- | Megaparsec's 'skipBlockComment' takes prefix and suffix and skips anything
 in between
-}
skipBlockComment :: Parser ()
skipBlockComment = L.skipBlockCommentNested "#|" "|#"

{- | Generate a parser for whitespace in a language with 'skipLineComment' and
 'skipBlockComment'
-}
spaces :: Parser ()
spaces = L.space space1 skipLineComment skipBlockComment

-- | Parse a 'Text' verbatim
symbol :: Text -> Parser Text
symbol = L.symbol spaces

-- | Parse an S-Expression
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | A 'lexeme' is ...
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaces

-- | The list of valid token characters, note that we allow invalid tokens at this point
validCharacter :: Parser Char
validCharacter =
  oneOf
    ( initialCharacter
        <> specialInitialCharacter
        <> digitCharacter
        <> specialSubsequentCharacter
    )

{- | A possibly empty tree of s-expressions

 Technically,
 @
 Token (SourcePos {..}, "hello")
 @

 isn't a valid s-expression. This is,

 @
 SExpression [Token (SourcePos {..}, "hello")]
 @

 and this is also valid,

 @
 SExpression []
 @

 The 'Data.Tree.Tree' type in containers is non-empty which isn't exactly what we are looking for

 TODO: can we have a smart constructor only?
-}
data SExpression
  = Token (SourcePos, Text)
  | SExpression [SExpression]
  deriving (Eq, Show)

-- | ...
term :: Parser SExpression
term = text <|> sExpression

-- | ...
text :: Parser SExpression
text = do
  sourcePosition <- getSourcePos
  token <- some validCharacter
  pure $ Token (sourcePosition, Text.pack token)

-- | ...
sExpression :: Parser SExpression
sExpression = SExpression <$> parens (term `sepEndBy` spaces)
