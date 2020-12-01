{-# LANGUAGE RecordWildCards #-}

module Text.Regex.Anagram.Compile
  ( compileAnagrex
  ) where

import           Control.Monad (mfilter)
import           Data.Function (on)
import qualified Data.IntSet as S
import           Data.List (sort)
import           Data.Maybe (mapMaybe)
import qualified Data.Vector as V

import Text.Regex.Anagram.Types
import Text.Regex.Anagram.Util

-- |Given a node count and a partial ordering (@before@) on a /sorted/ set of nodes (such that @before(node(i), node(j)) => i <= j@), construct a reduced (minimal) DAG
graphReduction :: Int -> (Int -> Int -> Bool) -> V.Vector S.IntSet
graphReduction n before = V.map fst $ V.constructN n $ \v ->
  let j = V.length v in V.ifoldr' (add j) (S.empty, S.singleton j) v
  where
  add j i (_, ic) rc@(r, c)
    | S.member i c = rc
    | before i j = (S.insert i r, S.union c ic)
    | otherwise = rc

charGraph :: [PatChar] -> Graph PatChar
charGraph l = Graph $ V.zip v $ graphReduction (V.length v) $ on subsetChar $ V.unsafeIndex v
  where
  v = V.fromList $ sort l

-- |Remove everything in patStars from patOpts
filterStar :: PatChars -> AnaPat
filterStar PatChars{..} = AnaPat
  { patFixed = chrStr fixed
  , patChars = PatChars
    { patReqs = charGraph reqs
    , patOpts = charGraph opts
    , patStar = patStar
    }
  , patMin = rlen
  , patMax = case patStar of
      PatSet s | S.null s -> Fin $ rlen + length opts
      _ -> Inf
  }
  where
  rlen = length patReqs
  (fixed, reqs) = partitionMaybe maybePatChr patReqs
  opts = mapMaybe (mfilter (not . nullChar) . Just . intersectChar (notChar patStar)) patOpts
  maybePatChr (PatChr c) = Just c
  maybePatChr _ = Nothing

compilePat :: PatChars -> AnaPat
compilePat = filterStar

compileAlts :: [PatChars] -> [AnaPat]
compileAlts = map compilePat

compileAnagrex :: [PatChars] -> Anagrex
compileAnagrex = Anagrex . compileAlts
