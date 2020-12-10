-- |The basic use of this module is to first parse a regular expression string (like @"[mn]{2,5}[eio]+s?"@) into an 'Anagrex' with 'makeAnagrex'.
-- This can then be used to efficiently test candidate strings with 'testAnagrex'.
module Text.Regex.Anagram
  ( Anagrex
  , makeAnagrex
  , makeAnagrexCI
  , testAnagrex
  , testAnagrexCI
  ) where

import qualified Data.CaseInsensitive as CI
import qualified Data.CaseInsensitive.Unsafe as CI

import Text.Regex.Anagram.Parse
import Text.Regex.Anagram.Compile
import Text.Regex.Anagram.Test

-- |Build a case-insensitive version of a pattern.
-- This is more efficient than 'CI.mk' since it avoids the complication of the case-sensitive version as well.
-- Uses 'CI.unsafeMk' so the 'CI.original' version is also case-folded.
makeAnagrexCI :: String -> Either String (CI.CI Anagrex)
makeAnagrexCI = fmap (CI.unsafeMk . compileAnagrex . CI.foldCase) . parseAnaPattern

-- |Test a case-insensitive pattern against a case-insensitive string.  You can also directly use 'Data.CaseInsensitive.foldCase' on both the pattern and the string to test.
testAnagrexCI :: CI.CI Anagrex -> CI.CI String -> Bool
testAnagrexCI p = testAnagrex (CI.foldedCase p) . CI.foldedCase
