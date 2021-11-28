module Snail.Types where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Display (Display (..), display)
import Data.Text.Lazy.Builder qualified as B
import Data.Vector (Vector (..))
import Data.Vector qualified as V

data AST
  = Nil
  | Expression Text
  | Boolean Bool
  | Number Integer
  | String Text
  | Quote AST
  | List (Vector AST)
  | Lambda (Vector Text) AST
  deriving (Eq)

instance Display AST where
  displayBuilder = \case
    Nil -> "Nil"
    Expression expr -> B.fromText expr
    Boolean True -> "#t"
    Boolean False -> "#f"
    Number int -> displayBuilder int
    String str -> "\"" <> B.fromText str <> "\""
    Quote ast -> "'" <> displayBuilder ast
    List contents -> "(" <> B.fromText (unwordVec (display <$> contents)) <> ")"
    Lambda contents ast -> "<lambda (" <> B.fromText (unwordVec (display <$> contents)) <> ") (" <> displayBuilder ast <> ")>"

unwordVec :: Vector Text -> Text
unwordVec vector = V.foldr1' (<>) $ intercalateVec " " vector

intercalateVec :: Text -> Vector Text -> Vector Text
intercalateVec separator vector =
  if V.null vector
    then vector
    else V.tail $ V.concatMap (\word -> V.fromList [separator, word]) vector
