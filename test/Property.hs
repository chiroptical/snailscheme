{-# LANGUAGE ScopedTypeVariables #-}

module Property where

import Data.Text (Text)
import Data.Text qualified as Text
import Snail.Parser
import Test.QuickCheck

genAtom :: Gen Text
genAtom = do
  first <- elements $ initialCharacter <> specialInitialCharacter
  rest <-
    listOf1 . elements $
      initialCharacter
        <> specialInitialCharacter
        <> specialSubsequentCharacter
        <> digitCharacter
  let notReserved = not . (`elem` reservedWords)
      candidateAtom = Text.pack $ first : rest
  pure candidateAtom `suchThat` notReserved

genBoolean :: Gen Text
genBoolean = elements ["true", "false"]
