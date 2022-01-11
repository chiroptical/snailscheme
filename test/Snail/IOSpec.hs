module Snail.IOSpec where

import Data.Either
import Snail.IO
import Test.Hspec

spec :: Spec
spec = do
  describe "successfully lexes some basic snail files" $ do
    it "lex a basic snail file" $ do
      eResults <- readSnailFile "examples/basic.snail"
      eResults `shouldSatisfy` isRight

    -- Not concerned with this at the lexing stage, we can
    -- error at the validation phase or just treat this
    -- as '()'
    it "lex an empty snail file" $ do
      eResults <- readSnailFile "examples/fail-empty.snail"
      eResults `shouldSatisfy` isLeft

    it "lex should fail with bad comment" $ do
      eResults <- readSnailFile "examples/fail-comment.snail"
      eResults `shouldSatisfy` isLeft

    it "lex should fail with non-terminated s-expression" $ do
      eResults <- readSnailFile "examples/fail.snail"
      eResults `shouldSatisfy` isLeft
