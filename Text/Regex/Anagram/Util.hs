{-# LANGUAGE TupleSections #-}

module Text.Regex.Anagram.Util
  where

import           Control.Applicative (Alternative, empty)
import           Data.Foldable (foldlM)
import           Data.Function (on)
import qualified Data.IntMap.Strict as M
import qualified Data.IntSet as S
import           Data.List (group, groupBy, sortOn)

import Text.Regex.Anagram.Types

guard' :: Alternative m => Bool -> a -> m a
guard' True = pure
guard' False = const empty

foldMapM :: (Monad m, Monoid b) => (a -> m b) -> [a] -> m b
-- foldMapM f = fmap fold . mapM f
foldMapM f = foldlM (\b a -> (b <>) <$> f a) mempty

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
-- concatMapM = foldMapM
concatMapM f = fmap concat . mapM f

partitionMaybe :: (a -> Maybe b) -> [a] -> ([b], [a])
partitionMaybe f = foldr sel ([], []) where
  sel x ~(j,n) = maybe (j,x:n) ((,n).(:j)) $ f x

rleLength :: RLE a -> Int
rleLength = foldl (\l (RL _ r) -> l + r) 0 . unRLE

rle :: Eq a => [a] -> RLE a
rle = RLE . map (\(x:l) -> RL x (succ $ length l)) . group

reRLE :: Eq a => RLE a -> RLE a
reRLE = RLE . map (\(RL x r:l) -> RL x (r + rleLength (RLE l))) . groupBy ((==) `on` unRL) . unRLE

sortRLE :: Ord a => RLE a -> RLE a
sortRLE = reRLE . RLE . sortOn unRL . unRLE

filterRLE :: (a -> Bool) -> RLE a -> RLE a
filterRLE f = RLE . filter (f . unRL) . unRLE

chrStr :: [Chr] -> ChrStr
chrStr = M.fromListWith (+) . map (, 1)

chrStrRLE :: ChrStr -> RLE Chr
chrStrRLE = RLE . map (uncurry RL) . M.toList

nullChar :: PatChar -> Bool
nullChar (PatSet s) = S.null s
nullChar _ = False

sizeChar :: PatChar -> Int
sizeChar (PatChr _) = 1
sizeChar (PatSet s) = S.size s
sizeChar (PatNot n) = maxBound - S.size n

notChar :: PatChar -> PatChar
notChar (PatChr c) = PatNot (S.singleton c)
notChar (PatSet s) = PatNot s
notChar (PatNot s) = PatSet s

intersectChr :: ChrSet -> PatChar -> PatChar
intersectChr s p@(PatChr c)
  | S.member c s = p
  | otherwise = mempty
intersectChr s (PatSet t) = PatSet $ S.intersection s t
intersectChr s (PatNot n) = PatSet $ S.difference s n

differenceChr :: ChrSet -> PatChar -> PatChar
differenceChr n p@(PatChr c)
  | S.member c n = mempty
  | otherwise = p
differenceChr n (PatSet s) = PatSet $ S.difference s n
differenceChr n (PatNot m) = PatNot $ S.union m n

intersectChar :: PatChar -> PatChar -> PatChar
intersectChar (PatSet s) p =  intersectChr s p
intersectChar (PatNot n) p = differenceChr n p
intersectChar p@(PatChr c) (PatChr d)
  | c == d = p
  | otherwise = mempty
intersectChar a b = intersectChar b a

differenceChar :: PatChar -> PatChar -> PatChar
differenceChar a b = intersectChar a (notChar b)

subsetChar :: PatChar -> PatChar -> Bool
subsetChar (PatChr c) (PatChr d) = c == d
subsetChar (PatChr c) (PatSet s) = S.member c s
subsetChar (PatChr c) (PatNot s) = S.notMember c s
subsetChar (PatSet s) (PatChr c) = S.null $ S.delete c s
subsetChar (PatSet s) (PatSet t) = S.isSubsetOf s t
subsetChar (PatSet s) (PatNot n) = S.disjoint s n
subsetChar (PatNot n) (PatNot m) = S.isSubsetOf m n
subsetChar _ _ = False
