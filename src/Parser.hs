module Parser where

import Control.Applicative hiding ((<|>))
import Data.Functor.Identity (Identity)
import Data.Text qualified as T
import LispVal
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language qualified as Language
import Text.Parsec.Text
import Text.Parsec.Token qualified as Token
import Text.ParserCombinators.Parsec.Token qualified as Token

-- Defined our token parser
lexer :: Token.GenTokenParser T.Text () Identity
lexer = Token.makeTokenParser style

-- specification for the lexeme
style :: Token.GenLanguageDef T.Text () Identity
style =
    Language.emptyDef
        { Token.commentStart = "#|"
        , Token.commentEnd = "|#"
        , Token.commentLine = ";"
        , Token.opStart = Token.opLetter style
        , Token.opLetter = oneOf ":!#$%%&*+./<=>?@\\^|-~"
        , Token.identStart = letter <|> oneOf "-+/*=|&><"
        , Token.identLetter = digit <|> letter <|> oneOf "?+=|&-/"
        , Token.reservedOpNames = ["'", "\""]
        }

-- Outstanding question: what the heck is this?
reservedOp :: T.Text -> Parser ()
reservedOp = Token.reservedOp lexer . T.unpack

-- What is an Atom?
parseAtom :: Parser LispVal
parseAtom = Atom . T.pack <$> Token.identifier lexer

parseText :: Parser LispVal
parseText = do
    reservedOp "\""
    p <- many1 $ noneOf "\""
    reservedOp "\""
    pure $ String . T.pack $ p

parseNumber :: Parser LispVal
parseNumber = do
    Text.Parsec.optional (char '+')
    d <- many1 digit
    pure $ Number . read $ d

parseNegNum :: Parser LispVal
parseNegNum = do
    char '-'
    d <- many1 digit
    pure $ Number . negate . read $ d

parseList :: Parser LispVal
parseList =
    List . concat <$> Text.Parsec.many parseExpr
        `sepBy` (char ' ' <|> char '\n')

parseSExp :: Parser LispVal
parseSExp =
    List . concat
        <$> Token.parens
            lexer
            ( Text.Parsec.many parseExpr
                `sepBy` (char ' ' <|> char '\n')
            )

parseQuote :: Parser LispVal
parseQuote = do
    reservedOp "\'"
    x <- parseExpr
    pure $ List [Atom "quote", x]

parseNil :: Parser LispVal
parseNil =
    ( reservedOp "Nil"
        <|> reservedOp "()"
        <|> reservedOp "'()"
    )
        >> pure Nil

parseReserved :: Parser LispVal
parseReserved =
    parseNil
        <|> (reservedOp "#t" >> pure (Bool True))
        <|> (reservedOp "#f" >> pure (Bool False))

parseExpr :: Parser LispVal
parseExpr =
    parseReserved
        <|> try parseNumber
        <|> try parseNegNum
        <|> parseAtom
        <|> parseText
        <|> parseQuote
        <|> parseSExp

contents :: Parser a -> Parser a
contents p = do
    Token.whiteSpace lexer
    r <- p
    eof
    pure r

readExpr :: T.Text -> Either ParseError LispVal
readExpr = parse (contents parseExpr) "<stdin>"

whitespace :: Parser ()
whitespace = Token.whiteSpace lexer

manyLispVal :: Parser [LispVal]
manyLispVal = parseExpr `sepBy` whitespace

readExprFile :: SourceName -> T.Text -> Either ParseError LispVal
readExprFile = parse (contents (List <$> manyLispVal))
