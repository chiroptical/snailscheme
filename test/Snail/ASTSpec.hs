{-# LANGUAGE TypeApplications #-}

module Snail.ASTSpec (spec) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Either (isRight)
import Data.Text qualified as Text
import Snail.IO (readSnailFile)
import Snail.Lexer qualified as Lexer
import Snail.Token (sExpressionToAST)
import Snail.Types
import Test.Hspec
import Test.QuickCheck
import Text.Megaparsec (SourcePos, initialPos)

spec :: Spec
spec = do
  describe "sExpressionToAST" $ do
    it "can parse basic s-expression with keyword" $ do
      forAll (arbitrary @Keywords) $ \keyword ->
        let keywordAsText = Text.toLower . Text.pack . show $ keyword
            sExpression = Lexer.SExpression [Lexer.Lexeme (position, keywordAsText)]
         in sExpressionToAST sExpression == Right (AST [Node (position, Keyword keyword)])

    it "can parse basic snail example to AST" $ do
      eResults <- liftIO $ readSnailFile "examples/basic.snail"
      eResults `shouldSatisfy` isRight
      let Right sExprs = eResults
          asts = sExpressionToAST <$> sExprs
      forM_ asts (`shouldSatisfy` isRight)

position :: SourcePos
position = initialPos "doesn't matter"
