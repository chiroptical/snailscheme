module Snail.Types where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Display (Display (..), display)
import Data.Text.Lazy.Builder qualified as B

data Expression
  = Nil
  | Atom Text
  | Boolean Bool
  | Number Integer
  | -- Maybe this should be StringLiteral to avoid confusion with Haskell's String
    String Text
  | Quote Expression
  | List [Expression]
  | -- (lambda (x y) (+ x y))
    Lambda [Text] Expression
  deriving (Eq)

instance Display Expression where
  displayBuilder = \case
    Nil -> "Nil"
    Atom expr -> B.fromText expr
    Boolean True -> "#t"
    Boolean False -> "#f"
    Number int -> displayBuilder int
    String str -> "\"" <> B.fromText str <> "\""
    Quote ast -> "'" <> displayBuilder ast
    List exprs -> "(" <> B.fromText (T.unwords (display <$> exprs)) <> ")"
    Lambda exprs ast -> "<lambda (" <> B.fromText (T.unwords (display <$> exprs)) <> ") (" <> displayBuilder ast <> ")>"

instance Show Expression where
  show = T.unpack . display
