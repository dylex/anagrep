{-# LANGUAGE RecordWildCards #-}

module Text.Regex.Anagram.Parse
  ( parseAnagrex
  ) where

import           Data.Foldable (fold)
import qualified Data.IntMap.Strict as M
import qualified Data.IntSet as S
import           Data.Semigroup (stimes)
import qualified Data.Set as Set
import qualified Text.Regex.TDFA.Pattern as R
import qualified Text.Regex.TDFA.ReadRegex as R

import Text.Regex.Anagram.Types
import Text.Regex.Anagram.Util
import Text.Regex.Anagram.Optimize

chrSet :: Set.Set Char -> ChrSet
chrSet = S.fromAscList . map fromEnum . Set.toAscList

unChars :: ChrStr -> [PatChar]
unChars = concatMap (uncurry $ flip replicate . PatChr) . M.toList where

charPat :: PatChar -> Pat
charPat (PatChr c) = mempty{ patChars = M.fromList [(c,1)], patMin = 1, patMax = 1 }
charPat p = mempty{ patSets = [p], patMin = 1, patMax = 1 }

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

starPat :: Pat -> PatChar
starPat Pat{..} = PatSet (M.keysSet patChars) <> fold patSets <> fold patOpts <> patStars

questPat :: Pat -> [PatChar]
questPat Pat{..} = unChars patChars ++ patSets ++ patOpts

makePattern :: R.Pattern -> Maybe Pat
makePattern (R.PGroup _ r) = makePattern r
makePattern (R.PNonCapture r) = makePattern r
makePattern (R.POr [r]) = makePattern r
makePattern (R.PConcat l) = foldMapM makePattern l
makePattern (R.PQuest r) = do
  p <- makePattern r
  return mempty{ patOpts = questPat p, patStars = patStars p, patMax = patMax p }
makePattern (R.PPlus r) = do
  p <- makePattern r
  return p{ patStars = starPat p, patMax = Inf * patMax p }
makePattern (R.PStar _ r) = do
  p <- makePattern r
  return mempty{ patStars = starPat p, patMax = Inf * patMax p }
makePattern (R.PBound i j' r) = do
  p <- makePattern r
  let ip = stimes i p
  return $ maybe
    ip{ patStars = starPat p, patMax = Inf * patMax p }
    (\j -> ip{ patOpts = patOpts ip ++ stimes (j - i) (questPat p), patMax = patMax ip + Fin (j - i) * patMax p })
    j'
makePattern R.PEmpty = return mempty
makePattern r = charPat <$> makeChar r

makeAlts :: R.Pattern -> Maybe [Pat]
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
makeAnagrex = fmap (optimizeAnagrex . Anagrex) . makeAlts

-- |Parse a string as a regular expression for matching anagrams, returning 'Left' error for invalid or unsupported regular expressions.  (Uses 'R.parseRegex'.)
parseAnagrex :: String -> Either String Anagrex
parseAnagrex r = case R.parseRegex r of
  Left e -> Left (show e)
  Right (p, _) -> maybe (Left "regexp contains features not supported for anagrams")
    Right $ makeAnagrex $ R.dfsPattern R.simplify' p

