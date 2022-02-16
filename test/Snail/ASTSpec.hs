module Snail.ASTSpec (spec) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Either (isRight)
import Snail.IO (readSnailFile)
import Snail.Lexer qualified as Lexer
import Snail.Token (sExpressionToAST)
import Snail.Types
import Test.Hspec
import Text.Megaparsec (SourcePos, initialPos)

spec :: Spec
spec = do
  describe "sExpressionToAST" $ do
    it "can parse basic sexpression with keyword" $ do
      let sExpression = Lexer.SExpression [Lexer.Lexeme (position, "if")]
          ast = sExpressionToAST sExpression
      ast `shouldBe` Right (AST [Node (position, Keyword If)])

    it "can parse and convert our basic example" $ do
      eResults <- liftIO $ readSnailFile "examples/basic.snail"
      eResults `shouldSatisfy` isRight
      let Right sExprs = eResults
          asts = sExpressionToAST <$> sExprs
      forM_ asts (`shouldSatisfy` isRight)

position :: SourcePos
position = initialPos "doesn't matter"
