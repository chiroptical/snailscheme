module Snail.IO where

import Data.Text.IO qualified as Text
import Snail.Lexer
import Text.Megaparsec

readSnailFile :: FilePath -> IO (Either String [SExpression])
readSnailFile fp = do
  contents <- Text.readFile fp
  putStrLn "....................."
  parseTest sExpressions contents
  putStrLn "....................."
  pure $ case parse sExpressions (show fp) contents of
    Left parseErrorBundle -> Left $ errorBundlePretty parseErrorBundle
    Right sexprs -> Right sexprs
