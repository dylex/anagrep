{-# LANGUAGE TupleSections #-}

module Text.Regex.Anagram.Util
  where

import           Data.Foldable (foldlM)
import qualified Data.IntMap.Strict as M
import qualified Data.IntSet as S

import Text.Regex.Anagram.Types

foldMapM :: (Monad m, Monoid b) => (a -> m b) -> [a] -> m b
-- foldMapM f = fmap fold . mapM f
foldMapM f = foldlM (\b a -> (b <>) <$> f a) mempty

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
-- concatMapM = foldMapM
concatMapM f = fmap concat . mapM f

chrStr :: String -> ChrStr
chrStr = M.fromListWith (+) . map ((, 1) . fromEnum)

nullChar :: PatChar -> Bool
nullChar (PatSet s) = S.null s
nullChar _ = False

notChar :: PatChar -> PatChar
notChar (PatChr c) = PatNot (S.singleton c)
notChar (PatSet s) = PatNot s
notChar (PatNot s) = PatSet s

intersectChar :: PatChar -> PatChar -> PatChar
intersectChar p@(PatChr c) (PatChr d)
  | c == d = p
  | otherwise = mempty
intersectChar p@(PatChr c) (PatSet s)
  | S.member c s = p
  | otherwise = mempty
intersectChar p@(PatChr c) (PatNot n)
  | S.member c n = mempty
  | otherwise = p
intersectChar (PatSet s) (PatSet t) = PatSet $ S.intersection s t
intersectChar (PatSet s) (PatNot n) = PatSet $ S.difference s n
intersectChar (PatNot n) (PatNot m) = PatSet $ S.union n m
intersectChar a b = intersectChar b a

differenceChar :: PatChar -> PatChar -> PatChar
differenceChar a b = intersectChar a (notChar b)

