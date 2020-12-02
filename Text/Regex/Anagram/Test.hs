{-# LANGUAGE RecordWildCards #-}

module Text.Regex.Anagram.Test
  ( testAnagrex
  ) where

import           Control.Arrow (second)
import           Control.Monad (MonadPlus, mfilter)
import           Control.Monad.ST (ST, runST)
import           Data.Foldable (fold)
import qualified Data.IntMap.Strict as M
import qualified Data.IntSet as S
import           Data.Maybe (isJust, isNothing)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

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
tryChars opt m0 (Graph g) = map fst $ V.foldM acc (m0, V.empty) g where
  acc (m, v) (c, p) = map (second $ V.snoc v . mappend excl) $ opts
    where
    excl = foldMap (V.unsafeIndex v) p
    opts
      | opt && null tc = [(m, excl)]
      | otherwise = tc
    tc = takeChar m excl c

testPat :: Int -> ChrStr -> AnaPat -> Bool
testPat l m0 AnaPat{ patChars = PatChars{..}, ..}
  | l < patMin = False
  | Fin l > patMax = False
  | otherwise = any M.null $ do
  ma <- subtractStr' m0 patFixed
  ml <- tryChars False ma patReqs
  mo <- tryChars True  ml patOpts
  return $ takeChars patStar mo

tryEach :: Monad m => M.IntMap a -> (ChrSet -> ChrStr -> m Bool) -> ChrSet -> ChrStr -> m (Maybe Bool)
tryEach ks next excl m
  | M.null ks = return Nothing
  | otherwise = tryl (M.keys ks) excl where
  tryl [] _ = return $ Just False
  tryl (c:l) e = do
    r <- next e $ M.update maybePred c m
    if r then return $ Just r else tryl l $ S.insert c e

tryChar :: Monad m => PatChar -> (ChrSet -> ChrStr -> m Bool) -> ChrSet -> ChrStr -> m (Maybe Bool)
tryChar (PatChr c) next excl m
  | S.member c excl = return Nothing
  | isJust j = Just <$> next excl m'
  | otherwise = return Nothing where
  (j, m') = M.updateLookupWithKey (\_ -> maybePred) c m
tryChar (PatSet s) next excl m = tryEach (M.restrictKeys m $ S.difference s excl) next excl m
tryChar (PatNot s) next excl m = tryEach (M.withoutKeys  m $ S.union      s excl) next excl m

tryChars' :: Bool -> Graph PatChar -> (ChrStr -> ST s Bool) -> ChrStr -> ST s Bool
tryChars' opt (Graph gr) next m0 = do
  v <- VM.new (V.length gr)
  let tryl [] m = next m
      tryl ((i, (c, p)):l) m
        | M.null m = return opt
        | otherwise = do
        excl <- foldMapM (VM.unsafeRead v) p
        let trye e m' = do
              VM.unsafeWrite v i e
              tryl l m'
        r <- tryChar c trye excl m
        maybe
          (if opt then trye S.empty m else return False)
          return
          r
  tryl (V.toList $ V.indexed gr) m0

testPat' :: Int -> ChrStr -> AnaPat -> Bool
testPat' l m0 AnaPat{ patChars = PatChars{..}, ..}
  | l < patMin = False
  | Fin l > patMax = False
  | any (0 >) ma = False
  | otherwise = runST $ do
  tryChars' False patReqs
    (tryChars' True patOpts $
      return . M.null . takeChars patStar)
    ma where
  ma = subtractStr m0 patFixed

-- |Check if any permutations of a string matches a parsed regular expression.  Always matches the full string.
testAnagrex :: Anagrex -> String -> Bool
testAnagrex (Anagrex l) s = any (testPat (length s) (chrStr $ map fromEnum s)) l
