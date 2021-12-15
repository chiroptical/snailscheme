module Property where

import Data.Text (Text)
import Data.Text qualified as Text
import Snail.Parser (validAtomCharacter)
import Test.QuickCheck

-- | TODO: Should define the list of characters allowed in symbols in Snail somewhere
genValidAtomCharacter :: Gen Char
genValidAtomCharacter = elements $ Text.unpack validAtomCharacter

genAtom :: Gen Text
genAtom = Text.pack <$> listOf genValidAtomCharacter
