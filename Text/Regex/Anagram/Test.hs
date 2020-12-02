{-# LANGUAGE RecordWildCards #-}

module Text.Regex.Anagram.Test
  ( testAnagrex
  ) where

import           Control.Arrow (second)
import           Control.Monad (MonadPlus, mfilter)
import           Data.Foldable (fold)
import qualified Data.IntMap.Strict as M
import qualified Data.IntSet as S
import           Data.Maybe (isJust, isNothing)
import qualified Data.Vector as V

import Text.Regex.Anagram.Types
import Text.Regex.Anagram.Util

maybePred :: Int -> Maybe Int
maybePred i
  | 1 < i = Just $ pred i
  | otherwise = Nothing

subtractStr :: ChrStr -> ChrStr -> ChrStr
subtractStr = M.mergeWithKey
  (\_ i j -> if i == j then Nothing else Just (i - j))
  id (fmap negate)

subtractStr' :: MonadPlus m => ChrStr -> ChrStr -> m ChrStr
subtractStr' a b = mfilter (all (0 <)) $ return $ subtractStr a b

mapAccum :: (a -> c -> (b, c)) -> c -> [a] -> [(b, c)]
mapAccum _ _ [] = []
mapAccum f z (a:l) = (b, z) : mapAccum f y l where
  (b, y) = f a z

subtractEach :: ChrStr -> M.IntMap a -> [(ChrStr, ChrSet)]
subtractEach m = mapAccum (\c s -> (M.update maybePred c m, S.insert c s)) S.empty . M.keys

takeChar :: ChrStr -> ChrSet -> PatChar -> [(ChrStr, ChrSet)]
takeChar m excl (PatChr c)
  | S.member c excl = []
  | isJust j = [(m', S.empty)]
  | otherwise = [] where
  (j, m') = M.updateLookupWithKey (\_ -> maybePred) c m
takeChar m excl (PatSet s) = subtractEach m $ M.restrictKeys m $ S.difference s excl
takeChar m excl (PatNot s) = subtractEach m $ M.withoutKeys  m $ S.union s excl

takeChars :: PatChar -> M.IntMap a -> M.IntMap a
takeChars (PatChr c) m = M.delete c m
takeChars (PatSet s) m = M.withoutKeys m s
takeChars (PatNot s) m = M.restrictKeys m s

tryChars :: Bool -> ChrStr -> Graph PatChar -> [ChrStr]
tryChars opt m0 = map fst . V.foldM acc (m0, V.empty) . unGraph where
  acc (m, v) (c, p) = map (second $ V.snoc v . mappend excl) $ opts
    where
    excl = foldMap (v V.!) p
    opts
      | opt && isNothing excl = tc ++ [(m, Nothing)]
      | otherwise = tc
    tc = map (second Just) $ takeChar m (fold excl) c

testPat :: Int -> ChrStr -> AnaPat -> Bool
testPat l m0 AnaPat{ patChars = PatChars{..}, ..}
  | l < patMin = False
  | Fin l > patMax = False
  | otherwise = any M.null $ do
  ma <- subtractStr' m0 patFixed
  ml <- tryChars False ma patReqs
  mo <- tryChars True  ml patOpts
  return $ takeChars patStar mo

-- |Check if any permutations of a string matches a parsed regular expression.  Always matches the full string.
testAnagrex :: Anagrex -> String -> Bool
testAnagrex (Anagrex l) s = any (testPat (length s) (chrStr $ map fromEnum s)) l
