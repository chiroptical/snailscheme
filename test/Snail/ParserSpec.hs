module Snail.ParserSpec (spec) where

import Control.Monad.IO.Class
import Data.Maybe (isJust)
import Data.Text qualified as Text
import Property
import Snail.Parser
import Snail.Types
import Test.Hspec
import Test.QuickCheck
import Text.Megaparsec
import Text.Megaparsec.Char

spec :: Spec
spec = do
  describe "Parsers: atom" $ do
    it "successfully parses any valid atoms into atoms" $ do
      forAll genAtom $ \s -> parseMaybe parseAtom s `shouldBe` Just (Atom s)
    it "successfully parses true and false" $ do
      forAll genBoolean $ \s -> parseMaybe parseAtom s `shouldSatisfy` isJust

  describe "Parsers: numbers" $ do
    it "fails to parse" $ do
      parseMaybe parseNumber "01a" `shouldBe` Nothing

  describe "Parsers: quote" $ do
    it "successfully parses a quoted atom" $ do
      parseMaybe parseQuote "'atom" `shouldBe` Just (Quote (Atom "atom"))
    it "successfully parses a quoted number" $ do
      parseMaybe parseQuote "'42" `shouldBe` Just (Quote (Number 42))
    it "successfully parses a quoted signed number" $ do
      parseMaybe parseQuote "'+42" `shouldBe` Just (Quote (Number 42))
    it "successfully parses a quoted signed negative number" $ do
      parseMaybe parseQuote "'-42" `shouldBe` Just (Quote (Number (-42)))

  describe "Parses: string literal" $ do
    it "successfully parses string literal" $ do
      parseMaybe parseStringLiteral "\"string-literal\"" `shouldBe` Just (StringLiteral "string-literal")
    it "successfully parses string literal with space" $ do
      parseMaybe parseStringLiteral "\"string literal\"" `shouldBe` Just (StringLiteral "string literal")

  describe "Parses: Nil" $ do
    it "successfully parses Nil" $ do
      parseMaybe parseNil "Nil" `shouldBe` Just Nil

  describe "Parses: terms" $ do
    it "fails to parse an invalid term" $ do
      parseMaybe parseTerm "01a" `shouldBe` Nothing

  describe "Parses: s-expressions" $ do
    -- TODO: Why does this fail? Test above fails, but this works...
    it "fails to parse an invalid s-expression" $ do
      parseMaybe parseSExpression "(01a)" `shouldBe` Nothing
    it "fails to parse an invalid s-expression" $ do
      parseMaybe parseSExpression "(01 a)" `shouldBe` Just (List [Number 1, Atom "a"])

  describe "Parses: expressions" $ do
    it "successfully parses padded number" $ do
      parseMaybe parseExpressions "(01 01)" `shouldBe` Just [List [Number 1, Number 1]]
    it "successfully parses padded number in a list" $ do
      parseMaybe parseExpressions "(01)" `shouldBe` Just [List [Number 1]]
