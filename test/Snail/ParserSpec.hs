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
import Text.Megaparsec.Char (eol)

spec :: Spec
spec = do
  describe "Parsers: atom" $ do
    it "successfully parses any valid atoms into atoms" $ do
      forAll genAtom $ \s -> parseMaybe parseAtom s `shouldBe` Just (Atom s)
    it "successfully parses true and false" $ do
      forAll genBoolean $ \s -> parseMaybe parseAtom s `shouldSatisfy` isJust

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

  describe "Parses: expressions" $ do
    it "successfully parses padded number" $ do
      parseMaybe parseExpression "01" `shouldBe` Just (Number 1)
    it "successfully parses padded number in a list" $ do
      parseMaybe parseExpression "(01)" `shouldBe` Just (List [Number 1])

-- TODO: Figure this one out...
-- it "successfully parses atom disguised as number at first" $ do
--   parseMaybe parseExpression "(01a)" `shouldBe` Just (List [Atom "01a"])
