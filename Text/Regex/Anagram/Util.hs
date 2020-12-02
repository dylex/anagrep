{-# LANGUAGE TupleSections #-}

module Text.Regex.Anagram.Util
  where

import           Control.Applicative (Alternative, empty)
import           Data.Foldable (foldlM)
import qualified Data.IntMap.Strict as M
import qualified Data.IntSet as S

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

chrStr :: [Chr] -> ChrStr
chrStr = M.fromListWith (+) . map (, 1)

nullChar :: PatChar -> Bool
nullChar (PatSet s) = S.null s
nullChar _ = False

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
