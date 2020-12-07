{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Text.Regex.Anagram.Test
  ( testAnagrex
  ) where

import           Control.Arrow (first)
import qualified Data.IntMap.Strict as M
import qualified Data.IntSet as S
import qualified Data.Vector as V

import Text.Regex.Anagram.Types
import Text.Regex.Anagram.Util
import Text.Regex.Anagram.Compile

import Text.Regex.Anagram.Parse
import Text.Regex.Anagram.Matrix

maybePred :: Int -> Maybe Int
maybePred i
  | 1 < i = Just $ pred i
  | otherwise = Nothing

predChr :: Chr -> ChrStr -> Maybe ChrStr
predChr c m = m' <$ j where
  (j, m') = M.updateLookupWithKey (\_ -> maybePred) c m

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
  | otherwise = maybe [] ((:[]) . (, excl)) $ predChr c m
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

reqChars :: ChrStr -> Graph PatChar -> [ChrStr]
reqChars = tryChars False
{-
reqChars m0 (Graph g) = ones vl M.empty m0
  where
  rest r m = ones' (M.toList r) m
  ones' [] r m = 
  ones [] r m = rest r m
  ones ((i,(p,_)):l) r m = tp p where
    tp (PatChr c) = maybe [] (ones l r) $ predChr c m
    tp ~(PatSet s) = case S.toAscList s of
      [] -> []
      [c] -> tp (PatChr c)
      sl -> ones l (M.unionWith (++) r $ M.fromAscList $ map (, [i]) sl) m
  vl = V.toList $ V.indexed gv
  gv = V.map (first $ intersectChr m0k) g
  m0k = M.keysSet m0
  -}

testPat :: Int -> ChrStr -> AnaPat -> Bool
testPat l m0 AnaPat{ patChars = PatChars{..}, ..}
  | l < patMin = False
  | Fin l > patMax = False
  | any (0 >) ma = False
  | otherwise = or $ do
  ml <- reqChars ma patReqs
  let ms = takeChars patStar ml
  mo <- tryChars True  ms patOpts
  return $ M.null mo
  where
  ma = subtractStr m0 patFixed

-- |Check if any permutations of a string matches a parsed regular expression.  Always matches the full string.
testAnagrex :: Anagrex -> String -> Bool
testAnagrex a@(Anagrex l) s = -- any (testPat (length s) (chrStr $ map fromEnum s)) l
  testAnaMatrix a s
