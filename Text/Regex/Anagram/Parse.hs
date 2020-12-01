{-# LANGUAGE RecordWildCards #-}

module Text.Regex.Anagram.Parse
  ( parseAnagrex
  ) where

import           Data.Foldable (fold)
import qualified Data.IntSet as S
import           Data.Semigroup (stimes)
import qualified Data.Set as Set
import qualified Text.Regex.TDFA.Pattern as R
import qualified Text.Regex.TDFA.ReadRegex as R

import Text.Regex.Anagram.Types
import Text.Regex.Anagram.Util
import Text.Regex.Anagram.Compile

import Debug.Trace

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

starPat :: PatChars -> PatChar
starPat PatChars{..} = fold patReqs <> fold patOpts <> patStar

questPat :: PatChars -> [PatChar]
questPat PatChars{..} = patReqs ++ patOpts

makePattern :: R.Pattern -> Maybe PatChars
makePattern (R.PGroup _ r) = makePattern r
makePattern (R.PNonCapture r) = makePattern r
makePattern (R.POr [r]) = makePattern r
makePattern (R.PConcat l) = foldMapM makePattern l
makePattern (R.PQuest r) = do
  p <- makePattern r
  return mempty{ patOpts = questPat p, patStar = patStar p }
makePattern (R.PPlus r) = do
  p <- makePattern r
  return p{ patStar = starPat p }
makePattern (R.PStar _ r) = do
  p <- makePattern r
  return mempty{ patStar = starPat p }
makePattern (R.PBound i j' r) = do
  p <- makePattern r
  let ip = stimes i p
  return $ maybe
    ip{ patStar = starPat p }
    (\j -> ip{ patOpts = patOpts ip ++ stimes (j - i) (questPat p) })
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

makeAnagrex :: R.Pattern -> Maybe Anagrex
makeAnagrex = fmap compileAnagrex . makeAlts

-- |Parse a string as a regular expression for matching anagrams, returning 'Left' error for invalid or unsupported regular expressions.  (Uses 'R.parseRegex'.)
parseAnagrex :: String -> Either String Anagrex
parseAnagrex r = case R.parseRegex r of
  Left e -> Left (show e)
  Right (p, _) -> maybe (Left "regexp contains features not supported for anagrams")
    Right $ makeAnagrex $ R.dfsPattern R.simplify' p

