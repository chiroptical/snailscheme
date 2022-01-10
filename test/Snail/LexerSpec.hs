{-# LANGUAGE OverloadedStrings #-}

module Snail.LexerSpec (spec) where

import Data.Maybe (isJust)
import Data.Text
import Property
import Snail.Lexer
import Test.Hspec
import Test.QuickCheck
import Text.Megaparsec (parseMaybe, parseTest)

foldTokens :: SExpression -> [Text]
foldTokens = go []
  where
    go :: [Text] -> SExpression -> [Text]
    go acc (Token (_, t)) = acc ++ [t]
    go acc (SExpression []) = acc
    go acc (SExpression (x : xs)) = lgo (go acc x) xs
    lgo :: [Text] -> [SExpression] -> [Text]
    lgo acc [] = acc
    lgo acc (x : xs) = lgo (go acc x) xs

sExpressionShouldBe :: Text -> [Text] -> Expectation
sExpressionShouldBe input output = do
  let mSExpr = parseMaybe sExpression input
  mSExpr `shouldSatisfy` isJust
  let Just sExpr = mSExpr
      tokens = foldTokens sExpr
  tokens `shouldBe` output

spec :: Spec
spec = do
  describe "parse sExpression" $ do
    it "successfully lex a basic list" $ do
      "(a b c)" `sExpressionShouldBe` ["a", "b", "c"]

    it "successfully lex a basic list" $ do
      "(1 a)" `sExpressionShouldBe` ["1", "a"]

    it "successfully lex a single element list" $ do
      "(1a)" `sExpressionShouldBe` ["1a"]

    it "successfully lex a nested s-expression" $ do
      "((1a))" `sExpressionShouldBe` ["1a"]

    it "successfully lex nested s-expressions" $ do
      "(() ())" `sExpressionShouldBe` []

    it "successfully lex a nested s-expressions" $ do
      "((()) (()))" `sExpressionShouldBe` []

    it "successfully lex line comment" $ do
      "(; ...\n)" `sExpressionShouldBe` []

    it "successfully lex line comment followed by token" $ do
      "(; ...\nabc)" `sExpressionShouldBe` ["abc"]

    it "successfully lex block comment" $ do
      "(#| ... |#)" `sExpressionShouldBe` []

    it "successfully lex block comment followed by token" $ do
      "(#| ... |#abc)" `sExpressionShouldBe` ["abc"]

    it "successfully lex token followed by block comments" $ do
      "(abc#| ... |#)" `sExpressionShouldBe` ["abc"]

    it "successfully lex block comments sorrounded by tokens" $ do
      "(abc#| ... |#def)" `sExpressionShouldBe` ["abc", "def"]

    it "successfully lex nested block comments" $ do
      "(#| ... #| ... |# ... |#)" `sExpressionShouldBe` []

    it "fail to lex nested block comments with missing internal start" $ do
      parseMaybe sExpression "(#| ... |# ... |#)" `shouldBe` Nothing

    it "fail to lex nested block comments with missing internal stop" $ do
      parseMaybe sExpression "(#| ... #| ... |#)" `shouldBe` Nothing

    it "fail to lex block comment with missing stop" $ do
      parseMaybe sExpression "(#| ...)" `shouldBe` Nothing
