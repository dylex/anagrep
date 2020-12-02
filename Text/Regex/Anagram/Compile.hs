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
import           Data.Function (on)
import qualified Data.IntSet as S
import           Data.List (sort)
import           Data.Maybe (mapMaybe)
import           Data.String (IsString(..))
import qualified Data.Vector as V

import Text.Regex.Anagram.Types
import Text.Regex.Anagram.Util
import Text.Regex.Anagram.Parse

-- |Compiled matching pattern
data AnaPat = AnaPat
  { patUncompiled :: PatChars -- ^original, uncompiled pattern (only for CI)
  , patFixed :: ChrStr -- ^required fixed chars (grouped 'PatChr')
  , patChars :: PatCharsOf Graph
  , patMin :: Int -- ^minimum length
  , patMax :: Inf Int -- ^maximum length
  } deriving (Show)

-- |A compiled regular expression pattern to match anagrams.
-- Represented as an (expanded) list of alternative 'AnaPat's.
newtype Anagrex = Anagrex [AnaPat]
  deriving (Show)

-- |Given a node count and a partial ordering (@before@) on a /sorted/ set of nodes (such that @before(node(i), node(j)) => i <= j@), construct a reduced (minimal) DAG
graphReduction :: Int -> (Int -> Int -> Bool) -> V.Vector [Int]
graphReduction n before = V.map fst $ V.constructN n $ \v ->
  let j = V.length v in V.ifoldr' (add j) ([], S.singleton j) v
  where
  add j i (_, ic) rc@(r, c)
    | S.member i c = rc
    | before i j = (i : r, S.union c ic)
    | otherwise = rc

charGraph :: [PatChar] -> Graph PatChar
charGraph l = Graph $ V.zip v $ graphReduction (V.length v) $ on subsetChar $ V.unsafeIndex v
  where
  v = V.fromList $ sort l

compilePat :: PatChars -> AnaPat
compilePat p@PatChars{..} = AnaPat
  { patUncompiled = p
  , patFixed = chrStr fixed
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

compileAlts :: [PatChars] -> [AnaPat]
compileAlts = map compilePat

compileAnagrex :: AnaPattern -> Anagrex
compileAnagrex (AnaPattern l) = Anagrex $ compileAlts l

-- |Build a regular expression for matching anagrams from a string, returning 'Left' error for invalid or unsupported regular expressions.  (Uses 'Text.Regex.TDFA.ReadRegex.parseRegex'.)
makeAnagrex :: String -> Either String Anagrex
makeAnagrex = fmap compileAnagrex . parseAnaPattern

instance FoldCase AnaPat where
  foldCase AnaPat{ patUncompiled = p } = compilePat $ foldCase p

instance FoldCase Anagrex where
  foldCase (Anagrex l) = Anagrex $ map foldCase l

instance IsString Anagrex where
  fromString = either error id . makeAnagrex

instance NFData AnaPat where
  rnf (AnaPat _ f c i j) = rnf f `seq` rnf c `seq` rnf i `seq` rnf j
instance NFData Anagrex where
  rnf (Anagrex l) = rnf l
