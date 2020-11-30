{-# LANGUAGE RecordWildCards #-}

module Text.Regex.Anagram.Test
  ( testAnagrex
  ) where

import           Control.Monad (MonadPlus, mfilter, foldM)
import qualified Data.IntMap.Strict as M
import           Data.Maybe (isJust)

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

subtractEach :: ChrStr -> M.IntMap a -> [ChrStr]
subtractEach m = map (\c -> M.update maybePred c m) . M.keys

takeChar :: ChrStr -> PatChar -> [ChrStr]
takeChar m (PatChr c)
  | isJust j = [m']
  | otherwise = [] where
  (j, m') = M.updateLookupWithKey (\_ -> maybePred) c m
takeChar m (PatSet s) = subtractEach m $ M.restrictKeys m s
takeChar m (PatNot s) = subtractEach m $ M.withoutKeys  m s

takeChars :: PatChar -> M.IntMap a -> M.IntMap a
takeChars (PatChr c) m = M.delete c m
takeChars (PatSet s) m = M.withoutKeys m s
takeChars (PatNot s) m = M.restrictKeys m s

testPat :: Int -> ChrStr -> Pat -> Bool
testPat l m0 Pat{..}
  | l < patMin = False
  | Fin l > patMax = False
  | otherwise = any M.null $ do
  ma <- subtractStr' m0 patChars
  ml <- foldM (                 takeChar)   ma patSets
  mo <- foldM (\m -> (++ [m]) . takeChar m) ml patOpts
  return $ takeChars patStars mo

-- |Check if any permutations of a string matches a parsed regular expression.  Always matches the full string.
testAnagrex :: Anagrex -> String -> Bool
testAnagrex (Anagrex l) s = any (testPat (length s) (chrStr s)) l
