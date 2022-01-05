{-# LANGUAGE OverloadedStrings #-}

module Snail.LexerSpec (spec) where

import Property
import Snail.Lexer
import Test.Hspec
import Test.QuickCheck
import Text.Megaparsec (parseMaybe, sepBy)

spec :: Spec
spec = do
  describe "parse tokens" $ do
    it "successfully lex a basic token" $ do
      parseMaybe potential "abc" `shouldBe` Just "abc"

    it "successfully lex a basic token" $ do
      parseMaybe sExpression "(a b c)" `shouldBe` Just ["a"]

-- describe "parse s-expression" $ do
--   it "successfully lex a basic s-expression" $ do
--     parseMaybe sExpression "(abc)" `shouldBe` Just ["abc"]
--   it "successfully lex a basic s-expression" $ do
--     parseMaybe sExpression "(1 a)" `shouldBe` Just ["1", "a"]
--   it "successfully lex a basic s-expression" $ do
--     parseMaybe sExpression "(1a)" `shouldBe` Just ["1a"]

--   describe "parse s-expressions" $ do
--     -- Comments
--     it "successfully lex line comments" $ do
--       parseMaybe sExpressions "(; ...)" `shouldBe` Just []
--     it "successfully lex block comments" $ do
--       parseMaybe sExpressions "(#| ... |#)" `shouldBe` Just []
--     it "successfully lex nested block comments" $ do
--       parseMaybe sExpressions "(#| ... #| ... |# ... |#)" `shouldBe` Just []
--     it "fail to lex nested block comments with missing internal start" $ do
--       parseMaybe sExpressions "(#| ... |# ... |#)" `shouldBe` Nothing
--     it "fail to lex nested block comments with missing internal stop" $ do
--       parseMaybe sExpressions "(#| ... #| ... |#)" `shouldBe` Nothing
--     it "fail to lex block comment with missing stop" $ do
--       parseMaybe sExpressions "(#| ...)" `shouldBe` Nothing
--
--     it "successfully lexes" $ do
--       parseMaybe sExpressions "(1a)" `shouldBe` Just [["1a"]]
--     it "parses" $ do
--       parseMaybe sExpressions "(1 a)" `shouldBe` Just [["1", "a"]]
