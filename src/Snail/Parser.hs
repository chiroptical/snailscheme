module Snail.Parser where

import Control.Monad.Combinators.Expr
import Control.Monad.IO.Class
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void
import Snail.Lexer (Parser)
import Snail.Lexer qualified as Lexer
import Snail.Types
import Text.Megaparsec
import Text.Megaparsec.Char

reservedWords :: [Text]
reservedWords =
  [ "true" --  "#t" - seems like 'true' 'false' is just easier to read?
  , "false" -- "#f"
  , "nil"
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

-- We only need this to parse plus/minus when they aren't attached to a number, e.g. (+ 2 1)
parseOperator :: Parser Expression
parseOperator = Atom . Text.pack . pure <$> oneOf ("+-" :: String)

parseAtom :: Parser Expression
parseAtom = do
  beginning <- parseInitialCharacter
  rest <- many parseSubsequentCharacter
  let atom = [beginning] <> rest
  pure $ case atom of
    "true" -> Boolean True
    "false" -> Boolean False
    "nil" -> Nil
    _ -> Atom $ Text.pack atom

parseStringLiteral :: Parser Expression
parseStringLiteral = do
  char '"'
  StringLiteral . Text.pack <$> manyTill printChar (char '"')

parseListOf :: Parser a -> Parser [a]
parseListOf p =
  Lexer.parens (p `sepBy` Lexer.spaces)

parseNumber :: Parser Expression
parseNumber = Number <$> Lexer.signedInteger

parseVariable :: Parser Text
parseVariable = Lexer.lexeme $ do
  beginning <- parseInitialCharacter
  rest <- many parseSubsequentCharacter
  let atom = [beginning] <> rest
  pure $ Text.pack atom

parseVariables :: Parser [Text]
parseVariables = parseListOf parseVariable

parseLambda :: Parser Expression
parseLambda = do
  Lexer.symbol "lambda"
  variables <- parseVariables
  Lambda variables <$> parseTerm

parseBinding :: Parser (Text, Expression)
parseBinding = Lexer.parens $ do
  variable <- parseVariable
  term <- parseTerm
  pure (variable, term)

parseBindings :: Parser [(Text, Expression)]
parseBindings = parseListOf parseBinding

parseLet :: Parser Expression
parseLet = do
  Lexer.symbol "let"
  bindings <- parseBindings
  Let bindings <$> parseTerm

parseIf :: Parser Expression
parseIf = do
  Lexer.symbol "if"
  predicate <- parseSExpression
  thenBranch <- parseTerm
  If predicate thenBranch <$> parseTerm

parseDefine :: Parser Expression
parseDefine = do
  Lexer.symbol "define"
  name <- parseVariable
  Define name <$> parseTerm

-- (lambda (x y)
--   (+ x y))
parseSExpression :: Parser Expression
parseSExpression =
  List <$> Lexer.parens (parseTerm `sepBy` Lexer.spaces)

parseQuote :: Parser Expression
parseQuote = do
  char '\''
  Quote <$> parseTerm

-- Maybe we can remove this try
-- http://blog.ezyang.com/2014/05/parsec-try-a-or-b-considered-harmful
parseTerm :: Parser Expression
parseTerm =
  (parseQuote <?> "Quote")
    <|> (parseAtom <?> "Atom")
    <|> (try parseNumber <?> "Number")
    <|> (parseOperator <?> "Operator")
    <|> (parseStringLiteral <?> "String Literal")
    <|> (parseSExpression <?> "SExpression")

parseExpressions :: Parser [Expression]
parseExpressions = many parseSExpression
