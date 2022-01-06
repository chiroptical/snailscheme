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
    go acc (SExpression (x : xs)) = lgo (go acc x) xs
    lgo :: [Text] -> [SExpression] -> [Text]
    lgo acc [] = acc
    lgo acc (x : xs) = lgo (go acc x) xs

spec :: Spec
spec = do
  describe "parse sExpression" $ do
    it "successfully lex a basic token" $ do
      let mSExpr = parseMaybe sExpression "(a b c)"
      mSExpr `shouldSatisfy` isJust
      let Just sExpr = mSExpr
          tokens = foldTokens sExpr
      tokens `shouldBe` ["a", "b", "c"]

--     it "successfully lex a basic s-expression" $ do
--       parseMaybe sExpression "(1 a)"
--         `shouldBe` Just (SExpression $ Token <$> ["1", "a"])
--
--     it "successfully lex a basic s-expression" $ do
--       parseMaybe sExpression "(1a)"
--         `shouldBe` Just (SExpression $ Token <$> ["1a"])
--
--     it "successfully lex a nested s-expression" $ do
--       parseMaybe sExpression "((1a))"
--         `shouldBe` Just (SExpression [SExpression $ Token <$> ["1a"]])
--
--     it "successfully lex a nested s-expressions" $ do
--       parseMaybe sExpression "(() ())"
--         `shouldBe` Just (SExpression [SExpression [], SExpression []])
--
--     it "successfully lex a nested s-expressions" $ do
--       parseMaybe sExpression "((()) (()))"
--         `shouldBe` Just (SExpression [SExpression [SExpression []], SExpression [SExpression []]])
--
--     it "successfully lex line comments" $ do
--       parseMaybe sExpression "(; ...\n)"
--         `shouldBe` Just (SExpression [])
--
--     it "successfully lex line comments" $ do
--       parseMaybe sExpression "(; ...\nabc)"
--         `shouldBe` Just (SExpression [Token "abc"])
--
--     it "successfully lex block comments" $ do
--       parseMaybe sExpression "(#| ... |#)"
--         `shouldBe` Just (SExpression [])
--
--     it "successfully lex block comments" $ do
--       parseMaybe sExpression "(#| ... |#abc)"
--         `shouldBe` Just (SExpression [Token "abc"])
--
--     it "successfully lex block comments" $ do
--       parseMaybe sExpression "(abc#| ... |#)"
--         `shouldBe` Just (SExpression [Token "abc"])
--
--     it "successfully lex nested block comments" $ do
--       parseMaybe sExpression "(#| ... #| ... |# ... |#)"
--         `shouldBe` Just (SExpression [])
--
--     it "fail to lex nested block comments with missing internal start" $ do
--       parseMaybe sExpression "(#| ... |# ... |#)" `shouldBe` Nothing
--
--     it "fail to lex nested block comments with missing internal stop" $ do
--       parseMaybe sExpression "(#| ... #| ... |#)" `shouldBe` Nothing
--
--     it "fail to lex block comment with missing stop" $ do
--       parseMaybe sExpression "(#| ...)" `shouldBe` Nothing
