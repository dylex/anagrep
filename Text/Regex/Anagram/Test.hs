{-# LANGUAGE RecordWildCards #-}

module Text.Regex.Anagram.Test
  ( testAnagrex
  ) where

import qualified Data.IntMap.Strict as M
import qualified Data.IntSet as S
import           Data.Maybe (isJust)
import qualified Data.Vector as V

import Text.Regex.Anagram.Types
import Text.Regex.Anagram.Util
import Text.Regex.Anagram.Compile

maybePred :: Int -> Maybe Int
maybePred i
  | 1 < i = Just $ pred i
  | otherwise = Nothing

subtractStr :: ChrStr -> ChrStr -> ChrStr
subtractStr = M.mergeWithKey
  (\_ i j -> if i == j then Nothing else Just (i - j))
  id (fmap negate)

mapAccum :: (a -> c -> (b, c)) -> c -> [a] -> [(b, c)]
mapAccum _ _ [] = []
mapAccum f z (a:l) = (b, z) : mapAccum f y l where
  (b, y) = f a z

tryEach :: ChrStr -> ChrSet -> M.IntMap a -> [(ChrStr, ChrSet)]
tryEach m excl = mapAccum (\c s -> (M.update maybePred c m, S.insert c s)) excl . M.keys

tryChar :: ChrStr -> ChrSet -> PatChar -> [(ChrStr, ChrSet)]
tryChar m excl (PatChr c)
  | S.member c excl = []
  | isJust j = [(m', excl)]
  | otherwise = [] where
  (j, m') = M.updateLookupWithKey (\_ -> maybePred) c m
tryChar m excl (PatSet s) = tryEach m excl $ M.restrictKeys m $ S.difference s excl
tryChar m excl (PatNot s) = tryEach m excl $ M.withoutKeys  m $ S.union s excl

takeChars :: PatChar -> M.IntMap a -> M.IntMap a
takeChars (PatChr c) m = M.delete c m
takeChars (PatSet s) m = M.withoutKeys m s
takeChars (PatNot s) m = M.restrictKeys m s

tryChars :: Bool -> ChrStr -> Graph PatChar -> [ChrStr]
tryChars opt m0 (Graph g) = tryl (V.toList g) m0 V.empty where
  tryl [] m _ = [m]
  tryl ((c, p):l) m v
    | M.null m = if opt then [m] else []
    | opt && null tc = tryl l m (V.snoc v excl)
    | otherwise = foldMap (\(m', e) -> tryl l m' (V.snoc v e)) tc
    where
    excl = foldMap (V.unsafeIndex v) p
    tc = tryChar m excl c

testPat :: Int -> ChrStr -> AnaPat -> Bool
testPat l m0 AnaPat{ patChars = PatChars{..}, ..}
  | l < patMin = False
  | Fin l > patMax = False
  | any (0 >) ma = False
  | otherwise = or $ do
  ml <- tryChars False ma patReqs
  mo <- tryChars True  ml patOpts
  return $ M.null $ takeChars patStar mo
  where
  ma = subtractStr m0 patFixed

-- |Check if any permutations of a string matches a parsed regular expression.  Always matches the full string.
testAnagrex :: Anagrex -> String -> Bool
testAnagrex (Anagrex l) s = any (testPat (length s) (chrStr $ map fromEnum s)) l
