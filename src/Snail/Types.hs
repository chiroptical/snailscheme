module Snail.Types where

import Data.Text (Text)
import Data.Text.Display (Display (..))
import Data.Text.Lazy.Builder qualified as B
import Data.Void (Void)
import Text.Megaparsec hiding (Token)

-- | TODO: 'Void' is the error type but we should probably use an explicit error type
type Parser = Parsec Void Text

data Keywords
  = Nil
  | Lambda
  | If
  | Let
  deriving (Eq, Show)

instance Display Keywords where
  displayBuilder = \case
    x -> displayBuilder $ show x

data Token
  = Atom Text
  | Boolean Bool
  | Number Integer
  | TextLiteral Text
  | Keyword Keywords
  deriving (Eq, Show)

instance Display Token where
  displayBuilder = \case
    Atom expr -> B.fromText expr
    Boolean True -> "true"
    Boolean False -> "false"
    Number int -> displayBuilder int
    TextLiteral str -> "\"" <> B.fromText str <> "\""
    Keyword keyword -> displayBuilder keyword
