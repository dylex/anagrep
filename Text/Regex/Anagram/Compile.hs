{-# LANGUAGE RecordWildCards #-}

module Text.Regex.Anagram.Compile
  ( AnaPat(..)
  , Anagrex(..)
  , compileAnagrex
  , makeAnagrex
  ) where

import           Control.DeepSeq (NFData(..))
import           Control.Monad (mfilter)
import           Data.CaseInsensitive (FoldCase(..))
import qualified Data.IntSet as S
import           Data.List (sort)
import           Data.Maybe (mapMaybe)
import           Data.String (IsString(..))

import Text.Regex.Anagram.Types
import Text.Regex.Anagram.Util
import Text.Regex.Anagram.Parse

-- |Compiled matching pattern
data AnaPat = AnaPat
  { patUncompiled :: PatChars -- ^original, uncompiled pattern (only for CI)
  , patChars :: PatCharsOf RLE
  , patMin :: Int -- ^minimum length
  , patMax :: Inf Int -- ^maximum length
  } deriving (Show)

-- |A compiled regular expression pattern to match anagrams.
-- Represented as an (expanded) list of alternative 'AnaPat's.
newtype Anagrex = Anagrex [AnaPat]
  deriving (Show)

compilePat :: PatChars -> AnaPat
compilePat p@PatChars{..} = AnaPat
  { patUncompiled = p
  , patChars = PatChars
    { patReqs = rle $ sort patReqs
    , patOpts = rle $ sort opts
    , patStar = patStar
    }
  , patMin = rlen
  , patMax = case patStar of
      PatSet s | S.null s -> Fin $ rlen + length opts
      _ -> Inf
  }
  where
  rlen = length patReqs
  opts = mapMaybe (mfilter (not . nullChar) . Just . intersectChar (notChar patStar)) patOpts

compileAlts :: [PatChars] -> [AnaPat]
compileAlts = map compilePat

-- |Compile an already-parsed 'AnaPattern' into an 'Anagrex'.
compileAnagrex :: AnaPattern -> Anagrex
compileAnagrex (AnaPattern l) = Anagrex $ compileAlts l

-- |Build a regular expression for matching anagrams from a string, returning 'Left' error for invalid or unsupported regular expressions.
-- (Uses 'Text.Regex.TDFA.ReadRegex.parseRegex'.)
-- This works by first expanding out a list of alternative patterns (like @"a|(b(c|d))"@ into @["a","bc","bd"]@) and then creating optimized pattern represenations for each.
makeAnagrex :: String -> Either String Anagrex
makeAnagrex = fmap compileAnagrex . parseAnaPattern

instance FoldCase AnaPat where
  foldCase AnaPat{ patUncompiled = p } = compilePat $ foldCase p

-- |Used to create a case-insensitive version of a pattern.
-- Note that this involves a re-compilation of the parsed 'AnaPattern'.  You can avoid this by using 'Text.Regex.Anagram.makeAnagrexCI'.
instance FoldCase Anagrex where
  foldCase (Anagrex l) = Anagrex $ map foldCase l

instance IsString Anagrex where
  fromString = either error id . makeAnagrex

instance NFData AnaPat where
  rnf (AnaPat _ c i j) = rnf c `seq` rnf i `seq` rnf j
instance NFData Anagrex where
  rnf (Anagrex l) = rnf l
