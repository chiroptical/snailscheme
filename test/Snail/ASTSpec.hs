module Snail.ASTSpec (spec) where

import Snail.Lexer qualified as Lexer
import Snail.Token
import Snail.Types
import Test.Hspec
import Text.Megaparsec

spec :: Spec
spec = do
  describe "sExpressionToAST" $ do
    it "can parse basic sexpression with keyword" $ do
      let sExpression = Lexer.SExpression [Lexer.Lexeme (position, "if")]
          ast = sExpressionToAST sExpression
      ast `shouldBe` Right (AST [Node (position, Keyword If)])

position :: SourcePos
position = initialPos "doesn't matter"
