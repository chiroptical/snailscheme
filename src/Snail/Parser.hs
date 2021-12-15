module Snail.Parser where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void
import Snail.Lexer
import Snail.Types
import Text.Megaparsec
import Text.Megaparsec.Char

validAtomCharacter :: String
validAtomCharacter = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "!@#$%^&*\\/?'\"+=-_<>{}[]~`"

parseAtomCharacter :: Parser Char
parseAtomCharacter = oneOf validAtomCharacter

-- TODO: Test this, I think it will end up failing on something like '01'
parseAtom :: Parser Expression
parseAtom = do
  beginning <- parseAtomCharacter
  rest <- many parseAtomCharacter
  let atom = [beginning] <> rest
  pure $ case atom of
    "#t" -> Boolean True
    "#f" -> Boolean False
    _ -> Atom $ Text.pack atom

parseText :: Parser Expression
parseText = do
  char '"'
  String . Text.pack <$> manyTill parseAtomCharacter (char '"')

parseNumber :: Parser Expression
parseNumber = Number <$> signedInteger

-- (lambda (x y)
--   (+ x y))
parseSExpression :: Parser Expression
parseSExpression =
  -- Is this `concat` semantically correct?
  List . concat <$> parens (many parseExpression `sepBy` spaces)

parseQuote :: Parser Expression
parseQuote = do
  char '\''
  Quote <$> parseExpression

parseNil :: Parser Expression
parseNil = do
  symbol "Nil"
  pure Nil

parseExpression :: Parser Expression
parseExpression =
  (parseNil <?> "Nil")
    <|> (try parseNumber <?> "Number")
    <|> (parseAtom <?> "Atom")
    <|> (parseText <?> "Text")
    <|> (parseQuote <?> "Quote")
    <|> (parseSExpression <?> "SExpression")
