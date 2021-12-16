module Property where

import Data.Text (Text)
import Data.Text qualified as Text
import Snail.Parser
import Test.QuickCheck

genValidAtomCharacter :: Gen Char
genValidAtomCharacter = elements validAtomCharacter

genAtom :: Gen Text
genAtom =
  let notReserved = not . (`elem` reservedWords)
      candidateAtom = Text.pack <$> listOf1 genValidAtomCharacter
   in candidateAtom `suchThat` notReserved

genBoolean :: Gen Text
genBoolean = elements ["true", "false"]
