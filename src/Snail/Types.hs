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
  | StringLiteral Text
  | Quote Expression
  | List [Expression]
  | -- (lambda (x y) (+ x y))
    Lambda [Text] Expression
  | -- (let ((x 1) (y 1)) (+ x y))
    Let [(Text, Expression)] Expression
  | -- (if (< x y) y x)
    If Expression Expression Expression
  | -- (define x 1)
    Define Text Expression
  deriving (Eq, Show)

instance Display Expression where
  displayBuilder = \case
    Nil -> "nil"
    Atom expr -> B.fromText expr
    Boolean True -> "true"
    Boolean False -> "false"
    Number int -> displayBuilder int
    StringLiteral str -> "\"" <> B.fromText str <> "\""
    Quote ast -> "'" <> displayBuilder ast
    List exprs -> "(" <> B.fromText (T.unwords (display <$> exprs)) <> ")"
    Lambda exprs ast ->
      "<lambda ("
        <> B.fromText (T.unwords (display <$> exprs))
        <> ") ("
        <> displayBuilder ast
        <> ")>"
    Let bindings ast ->
      "let ("
        <> B.fromText (T.unwords (display <$> bindings))
        <> ") ("
        <> displayBuilder ast
        <> ")>"
    If expr then' else' ->
      "if ("
        <> displayBuilder expr
        <> ") then ("
        <> displayBuilder then'
        <> ") else ("
        <> displayBuilder else'
        <> ")"
