module Snail.Token where

import Data.Text (Text)
import Data.Text qualified as Text
import Snail.Characters
import Snail.Lexer qualified as Lexer
import Snail.Types
import Text.Megaparsec hiding (Token, token)

-- | ...
parseInitialCharacter :: Parser Char
parseInitialCharacter = oneOf $ initialCharacter <> specialInitialCharacter

-- | ...
parseSubsequentCharacter :: Parser Char
parseSubsequentCharacter = parseInitialCharacter <|> oneOf (specialSubsequentCharacter <> digitCharacter)

-- | ...
parseAtom :: Parser Token
parseAtom = do
  beginning <- parseInitialCharacter
  rest <- many parseSubsequentCharacter
  let atom = [beginning] <> rest
  pure $ case atom of
    "true" -> Boolean True
    "false" -> Boolean False
    "nil" -> Keyword Nil
    "if" -> Keyword If
    "let" -> Keyword Let
    "lambda" -> Keyword Lambda
    _ -> Atom $ Text.pack atom

-- | ...
parseNumber :: Parser Token
parseNumber = do
  sign <- optional $ oneOf ("+-" :: String)
  number <- read <$> some (oneOf digitCharacter)
  pure $ case sign of
    Just '-' -> Number (negate number)
    _ -> Number number

-- | ...
parseTerm :: Parser Token
parseTerm = try parseNumber <|> parseAtom

-- | ...
data ASTException
  = UnableToParseLexeme Text SourcePos
  deriving (Eq, Show)

{- | ...
 'Data.Either.Validation' might work better here...
-}
sExpressionToAST :: Lexer.SExpression -> Either [ASTException] AST
sExpressionToAST = \case
  Lexer.TextLiteral (sourcePosition, text) ->
    Right $ Node (sourcePosition, TextLiteral text)
  Lexer.Lexeme (sourcePosition, text) ->
    let result = parseMaybe parseTerm text
     in case result of
          Nothing -> Left [UnableToParseLexeme text sourcePosition]
          Just token -> Right $ Node (sourcePosition, token)
  Lexer.SExpression xs -> AST <$> traverse sExpressionToAST xs
