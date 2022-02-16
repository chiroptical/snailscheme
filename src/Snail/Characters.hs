module Snail.Characters where

-- | ...
initialCharacter :: String
initialCharacter = ['a' .. 'z'] <> ['A' .. 'Z']

-- | ...
specialInitialCharacter :: String
specialInitialCharacter = "!$%&*/:<=>?^_~"

-- | ...
digitCharacter :: String
digitCharacter = ['0' .. '9']

-- | ...
specialSubsequentCharacter :: String
specialSubsequentCharacter = "+-.@"
