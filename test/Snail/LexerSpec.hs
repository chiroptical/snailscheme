module Snail.LexerSpec (spec) where

import Data.Text qualified as T
import Property
import Snail.Lexer
import Test.Hspec
import Test.QuickCheck
import Text.Megaparsec

spec :: Spec
spec = do
    describe "Parsing comments" $ do
        it "successfully parses line comments" $ do
            parseMaybe skipLineComment "; ..." `shouldBe` Just ()
        it "successfully parses block comments" $ do
            parseMaybe skipBlockComment "#| ... |#" `shouldBe` Just ()
        it "successfully parses nested block comments" $ do
            parseMaybe skipBlockComment "#| ... #| ... #| ... |#" `shouldBe` Just ()
        it "successfully parses nested block comments with missing internal stop" $ do
            parseMaybe skipBlockComment "#| ... #| ... |#" `shouldBe` Just ()
        it "fails to parse block comment with missing stop" $ do
            parseMaybe skipBlockComment "#| ..." `shouldBe` Nothing

    describe "Parsing symbols" $ do
        it "successfully parses any valid symbol" $ do
            forAll genSymbol $ \s -> parseMaybe (symbol s) s `shouldBe` Just s

    describe "Signed and unsigned integers" $ do
        let tshow = T.pack . show
        it "successfully parses positive and negative integers" $ do
            forAll (arbitrary @Integer) $
                \i -> parseMaybe signedInteger (tshow i) `shouldBe` Just i
        it "successfully parses positive signed integers" $ do
            forAll (arbitrary @Integer) $
                \i -> parseMaybe signedInteger ("+ " <> tshow (abs i)) `shouldBe` Just (abs i)

    describe "Parsing parens" $ do
        it "fails to parse parens of line comment" $ do
            parseMaybe (parens skipLineComment) "( ; ... )" `shouldBe` Nothing
        it "successfully parses parens of integer" $ do
            parseMaybe (parens signedInteger) "(42)" `shouldBe` Just 42
            parseMaybe (parens signedInteger) "( 42 )" `shouldBe` Just 42
            parseMaybe (parens signedInteger) "(-42)" `shouldBe` Just (-42)
            parseMaybe (parens signedInteger) "( -42 )" `shouldBe` Just (-42)
        it "successfully parses parens of positive signed integer" $ do
            parseMaybe (parens signedInteger) "(+42)" `shouldBe` Just 42
            parseMaybe (parens signedInteger) "( +42 )" `shouldBe` Just 42
        it "successfully parses parens of integer with a block comment" $ do
            parseMaybe (parens signedInteger) "( #|...|# 42 )" `shouldBe` Just 42
            parseMaybe (parens signedInteger) "( 42 #|...|# )" `shouldBe` Just 42
