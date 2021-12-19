module Snail.ParserSpec (spec) where

import Control.Monad.IO.Class
import Data.Maybe (isJust)
import Data.Text qualified as Text
import Property
import Snail.Lexer
import Snail.Parser
import Snail.Types
import Test.Hspec
import Test.QuickCheck
import Text.Megaparsec
import Text.Megaparsec.Char

spec :: Spec
spec = do
  describe "atoms" $ do
    it "successfully parses any valid atoms into atoms" $ do
      forAll genAtom $ \s -> parseMaybe parseAtom s `shouldBe` Just (Atom s)
    it "successfully parses true and false" $ do
      forAll genBoolean $ \s -> parseMaybe parseAtom s `shouldSatisfy` isJust
    it "successfully parses nil" $ do
      parseMaybe parseAtom "nil" `shouldBe` Just Nil

  describe "numbers" $ do
    it "fails to parse" $ do
      parseMaybe parseNumber "01a" `shouldBe` Nothing

  describe "quotes" $ do
    it "successfully parses a quoted atom" $ do
      parseMaybe parseQuote "'atom" `shouldBe` Just (Quote (Atom "atom"))
    it "successfully parses a quoted number" $ do
      parseMaybe parseQuote "'42" `shouldBe` Just (Quote (Number 42))
    it "successfully parses a quoted signed number" $ do
      parseMaybe parseQuote "'+42" `shouldBe` Just (Quote (Number 42))
    it "successfully parses a quoted signed negative number" $ do
      parseMaybe parseQuote "'-42" `shouldBe` Just (Quote (Number (-42)))

  describe "string literals" $ do
    it "successfully parses string literal" $ do
      parseMaybe parseStringLiteral "\"string-literal\"" `shouldBe` Just (StringLiteral "string-literal")
    it "successfully parses string literal with space" $ do
      parseMaybe parseStringLiteral "\"string literal\"" `shouldBe` Just (StringLiteral "string literal")

  describe "terms" $ do
    it "fails to parse an invalid term" $ do
      parseMaybe parseTerm "01a" `shouldBe` Nothing

  describe "lambdas" $ do
    it "successfully parses a valid lambda" $ do
      parseMaybe parseLambda "lambda (x y) (+ x y)"
        `shouldBe` Just (Lambda ["x", "y"] (List [Atom "+", Atom "x", Atom "y"]))
    it "fails to parse variables with nested s-expression" $ do
      parseMaybe parseLambda "lambda (x (y)) (+ x y)" `shouldBe` Nothing
    it "fails to parse variables that are numbers" $ do
      parseMaybe parseLambda "lambda (x 1) (+ x y)" `shouldBe` Nothing
    it "fails to parse incomplete lambda" $ do
      parseMaybe parseLambda "lambda (+ x y)" `shouldBe` Nothing
    it "fails to parse lambda where variables come second" $ do
      parseMaybe parseLambda "lambda (+ x y) (x y)" `shouldBe` Nothing
    it "successfully parses a constant lambda" $ do
      parseMaybe parseLambda "lambda (x y) 1"
        `shouldBe` Just (Lambda ["x", "y"] (Number 1))

  describe "binding/s" $ do
    it "successfully parses a binding" $ do
      parseMaybe parseBinding "(x 1)" `shouldBe` Just ("x", Number 1)
    it "successfully parses bindings" $ do
      parseMaybe parseBindings "((x 1) (y 2))" `shouldBe` Just [("x", Number 1), ("y", Number 2)]

  describe "let" $ do
    it "successfully parses a valid let" $ do
      parseMaybe parseLet "let ((x 1) (y 2)) (+ x y)"
        `shouldBe` Just (Let [("x", Number 1), ("y", Number 2)] (List [Atom "+", Atom "x", Atom "y"]))
    it "successfully parses a constant let" $ do
      parseMaybe parseLet "let ((x 1) (y 2)) 1"
        `shouldBe` Just (Let [("x", Number 1), ("y", Number 2)] (Number 1))

  describe "if" $ do
    it "successfully parses a valid if" $ do
      parseMaybe parseIf "if (> 2 1) 1 2"
        `shouldBe` Just (If (List [Atom ">", Number 2, Number 1]) (Number 1) (Number 2))

  describe "define" $ do
    it "successfully parses a valid define" $ do
      parseTest parseDefine "define x 1"
      parseMaybe parseDefine "define x 1"
        `shouldBe` Just (Define "x" (Number 1))

  describe "s-expressions" $ do
    it "successfully parses a valid s-expression" $ do
      parseMaybe parseSExpression "(01 a)" `shouldBe` Just (List [Number 1, Atom "a"])
    it "successfully parses a valid s-expression" $ do
      parseMaybe parseSExpression "(+ x y)" `shouldBe` Just (List [Atom "+", Atom "x", Atom "y"])

  describe "expressions" $ do
    it "successfully parses padded number" $ do
      parseMaybe parseExpressions "(01 01)" `shouldBe` Just [List [Number 1, Number 1]]
    it "successfully parses padded number in a list" $ do
      parseMaybe parseExpressions "(01)" `shouldBe` Just [List [Number 1]]

  -- Known edge cases are things we would like to fix in the future as long as
  -- we don't increase the complexity of the implementation. We might try
  -- to look into other parsing libraries to see if there is a cheap way to fix
  -- these.
  describe "known edge cases" $ do
    -- Interesting tidbit, a similar parse issue exists in Haskell
    it "should fail to parse this, but doesn't" $
      parseMaybe parseSExpression "(01a)" `shouldBe` Just (List [Number 1, Atom "a"])
