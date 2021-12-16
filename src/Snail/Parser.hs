module Snail.Parser where

import Control.Monad.Combinators.Expr
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void
import Snail.Lexer
import Snail.Types
import Text.Megaparsec
import Text.Megaparsec.Char

reservedWords :: [Text]
reservedWords =
  [ "true"
  , "false"
  ]

validAtomCharacter :: String
validAtomCharacter = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "!@#$%^&*\\/?'\"+=-_<>{}[]~`"

parseAtomCharacter :: Parser Char
parseAtomCharacter = oneOf validAtomCharacter

parseAtom :: Parser Expression
parseAtom = do
  beginning <- parseAtomCharacter
  rest <- many parseAtomCharacter
  let atom = [beginning] <> rest
  pure $ case atom of
    "true" -> Boolean True
    "false" -> Boolean False
    _ -> Atom $ Text.pack atom

parseStringLiteral :: Parser Expression
parseStringLiteral = do
  char '"'
  StringLiteral . Text.pack <$> manyTill (parseAtomCharacter <|> spaceChar) (char '"')

parseNumber :: Parser Expression
parseNumber = Number <$> signedInteger

-- (lambda (x y)
--   (+ x y))
parseSExpression :: Parser Expression
parseSExpression =
  -- Is this `concat` semantically correct?
  List . concat <$> parens (many parseExpression `sepBy` spaceChar)

parseQuote :: Parser Expression
parseQuote = do
  char '\''
  Quote <$> parseTerm

parseNil :: Parser Expression
parseNil = symbol "Nil" >> pure Nil

parseTerm :: Parser Expression
parseTerm =
  (parseNil <?> "Nil")
    <|> (parseQuote <?> "Quote")
    <|> (try parseNumber <?> "Number")
    <|> (parseAtom <?> "Atom")
    <|> (parseStringLiteral <?> "String Literal")
    <|> (parseSExpression <?> "SExpression")

parseExpression :: Parser Expression
parseExpression = makeExprParser parseTerm [] <?> "Expression"
