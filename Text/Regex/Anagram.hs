module Text.Regex.Anagram
  ( Anagrex
  , AnaPattern
  , parseAnaPattern
  , compileAnagrex
  , makeAnagrex
  , testAnagrex
  -- , testAnagrexCI
  ) where

import           Data.CaseInsensitive (CI, foldedCase, foldCase)

import Text.Regex.Anagram.Types
import Text.Regex.Anagram.Parse
import Text.Regex.Anagram.Compile
import Text.Regex.Anagram.Test

-- |Build a regular expression for matching anagrams from a string, returning 'Left' error for invalid or unsupported regular expressions.  (Uses 'Text.Regex.TDFA.ReadRegex.parseRegex'.)
makeAnagrex :: String -> Either String Anagrex
makeAnagrex = fmap compileAnagrex . parseAnaPattern

{-
instance IsString Anagrex where
  fromString = either error id . makeAnagrex
  -}

-- |Build a case-insensitive 'Anagrex', which can only be applied to 'foldedCase' strings.
makeAnagrexCI :: String -> Either String Anagrex
makeAnagrexCI = fmap (compileAnagrex . foldCase) . parseAnaPattern

{-
-- |Test a case-insensitive pattern against a case-insensitive string.  You can also directly use 'Data.CaseInsensitive.foldCase' on both the pattern and the string to test.
testAnagrexCI :: CI Anagrex -> CI String -> Bool
testAnagrexCI p = testAnagrex (foldedCase p) . foldedCase
-}
