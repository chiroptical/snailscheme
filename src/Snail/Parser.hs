module Snail.Parser where

import Control.Monad.Combinators.Expr
import Control.Monad.IO.Class
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void
import Snail.Lexer
import Snail.Types
import Text.Megaparsec
import Text.Megaparsec.Char

reservedWords :: [Text]
reservedWords =
  [ "true" --  "#t" - seems like 'true' 'false' is just easier to read?
  , "false" -- "#f"
  ]

-- Reference: https://people.csail.mit.edu/jaffer/r5rs/Lexical-structure.html#Lexical-structure
-- Technically, 'A'..'Z' isn't valid but seems fine?
initialCharacter :: String
initialCharacter = ['a' .. 'z'] <> ['A' .. 'Z']

specialInitialCharacter :: String
specialInitialCharacter = "!$%&*/:<=>?^_~"

digitCharacter :: String
digitCharacter = ['0' .. '9']

specialSubsequentCharacter :: String
specialSubsequentCharacter = "+-.@"

parseInitialCharacter :: Parser Char
parseInitialCharacter = oneOf $ initialCharacter <> specialInitialCharacter

parseSubsequentCharacter :: Parser Char
parseSubsequentCharacter = parseInitialCharacter <|> oneOf (specialSubsequentCharacter <> digitCharacter)

parseAtom :: Parser Expression
parseAtom = do
  beginning <- parseInitialCharacter
  rest <- many parseSubsequentCharacter
  let atom = [beginning] <> rest
  pure $ case atom of
    "true" -> Boolean True
    "false" -> Boolean False
    _ -> Atom $ Text.pack atom

parseStringLiteral :: Parser Expression
parseStringLiteral = do
  char '"'
  StringLiteral . Text.pack <$> manyTill printChar (char '"')

-- Consider the edge case `01a`, the parser will pull 01 off the front
-- as a valid integer. However, this is not a valid token in Snailscheme.
parseNumber :: Parser Expression
parseNumber = Number <$> signedInteger <* lookAhead space

-- (lambda (x y)
--   (+ x y))
parseSExpression :: Parser Expression
parseSExpression =
  -- Is this `concat` semantically correct?
  List . concat <$> parens (many parseTerm `sepBy` space1)

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
    <|> (parseAtom <?> "Atom")
    <|> (parseNumber <?> "Number")
    <|> (parseStringLiteral <?> "String Literal")
    <|> (parseSExpression <?> "SExpression")

parseExpressions :: Parser [Expression]
parseExpressions = many parseSExpression
