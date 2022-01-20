module Snail.Tokens where

import Data.Text qualified as Text
import Snail.Characters
import Snail.Types
import Text.Megaparsec hiding (Token)

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
