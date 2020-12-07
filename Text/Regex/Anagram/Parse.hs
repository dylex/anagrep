{-# LANGUAGE RecordWildCards #-}

module Text.Regex.Anagram.Parse
  ( AnaPattern(..)
  , parseAnaPattern
  ) where

import           Data.CaseInsensitive (FoldCase(..))
import qualified Data.IntSet as S
import qualified Data.Set as Set
import           Data.String (IsString(..))
import qualified Text.Regex.TDFA.Pattern as R
import qualified Text.Regex.TDFA.ReadRegex as R

import Text.Regex.Anagram.Types
import Text.Regex.Anagram.Util

-- |A parsed intermediate representation of regular expression pattern to match anagrams.
-- This is exposed mainly to make case-insensitive matches more efficient, so that 'foldCase' can be performed on the 'AnaPattern' to avoid unnecessary compilation.
-- Represented as an (expanded) list of alternative 'PatChars'.
newtype AnaPattern = AnaPattern [PatChars]
  deriving (Show)

chrSet :: Set.Set Char -> ChrSet
chrSet = S.fromAscList . map fromEnum . Set.toAscList

charPat :: PatChar -> PatChars
charPat p = mempty{ patReqs = [p] }

makeChar :: R.Pattern -> Maybe PatChar
makeChar R.PDot{} = return $ PatNot S.empty
makeChar R.PChar{ R.getPatternChar = c } = return $ PatChr $ fromEnum c
makeChar R.PEscape{ R.getPatternChar = c }
  | c `notElem` "`'<>bB" = return $ PatChr $ fromEnum c
-- TODO: use R.decodePatternSet
makeChar R.PAny{ R.getPatternSet = R.PatternSet (Just s) Nothing Nothing Nothing } =
  return $ PatSet $ chrSet s
makeChar R.PAnyNot{ R.getPatternSet = R.PatternSet (Just s) Nothing Nothing Nothing } =
  return $ PatNot $ chrSet s
makeChar _ = Nothing

makePattern :: R.Pattern -> Maybe PatChars
makePattern (R.PGroup _ r) = makePattern r
makePattern (R.PNonCapture r) = makePattern r
makePattern (R.POr [r]) = makePattern r
makePattern (R.PConcat l) = foldMapM makePattern l
makePattern (R.PQuest r) = do
  p <- makeChar r
  return mempty{ patOpts = [p] }
makePattern (R.PPlus r) = do
  p <- makeChar r
  return mempty{ patReqs = [p], patStar = p }
makePattern (R.PStar _ r) = do
  p <- makeChar r
  return mempty{ patStar = p }
makePattern (R.PBound i j' r) = do
  p <- makeChar r
  let ip = mempty{ patReqs = replicate i p }
  return $ maybe
    ip{ patStar = p }
    (\j -> ip{ patOpts = replicate (j - i) p })
    j'
makePattern R.PEmpty = return mempty
makePattern r = charPat <$> makeChar r

makeAlts :: R.Pattern -> Maybe [PatChars]
makeAlts (R.PGroup _ r) = makeAlts r
makeAlts (R.PNonCapture r) = makeAlts r
makeAlts (R.POr o) = concatMapM makeAlts o
makeAlts (R.PConcat c) = cross <$> mapM makeAlts c where
  cross [] = [mempty]
  cross (l:r) = do
    a <- l
    b <- cross r
    return (a <> b)
makeAlts r = return <$> makePattern r

-- |Parse a string as a regular expression for matching anagrams, returning 'Left' error for invalid or unsupported regular expressions.  (Uses 'R.parseRegex'.)
parseAnaPattern :: String -> Either String AnaPattern
parseAnaPattern r = case R.parseRegex r of
  Left e -> Left (show e)
  Right (p, _) -> maybe (Left "regexp contains features not supported for anagrams")
    (Right . AnaPattern) $ makeAlts $ R.dfsPattern R.simplify' p

instance FoldCase AnaPattern where
  foldCase (AnaPattern l) = AnaPattern (map foldCase l)

instance IsString AnaPattern where
  fromString = either error id . parseAnaPattern
