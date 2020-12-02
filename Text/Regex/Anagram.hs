module Text.Regex.Anagram
  ( Anagrex
  , AnaPattern
  , parseAnaPattern
  , compileAnagrex
  , makeAnagrex
  , testAnagrex
  , testAnagrexCI
  ) where

import           Data.CaseInsensitive (CI, foldedCase)

import Text.Regex.Anagram.Parse
import Text.Regex.Anagram.Compile
import Text.Regex.Anagram.Test

-- |Test a case-insensitive pattern against a case-insensitive string.  You can also directly use 'Data.CaseInsensitive.foldCase' on both the pattern and the string to test.
testAnagrexCI :: CI Anagrex -> CI String -> Bool
testAnagrexCI p = testAnagrex (foldedCase p) . foldedCase
