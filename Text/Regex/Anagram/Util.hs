{-# LANGUAGE TupleSections #-}

module Text.Regex.Anagram.Util
  where

import           Control.Applicative (Alternative, empty)
import           Data.Foldable (foldlM)
import           Data.Function (on)
import qualified Data.IntMap.Strict as M
import qualified Data.IntSet as S
import           Data.List (group, groupBy, sortOn)
import qualified Data.Vector as V
import qualified Data.Vector.Fusion.Bundle as VB
import qualified Data.Vector.Fusion.Bundle.Size as VBS
import qualified Data.Vector.Fusion.Stream.Monadic as VS
import qualified Data.Vector.Generic as VG

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

{-# INLINE withRLE #-}
withRLE :: (f (RL a) -> g (RL b)) -> RLEof f a -> RLEof g b
withRLE f = RLE . f . unRLE

rleLength :: RLE a -> Int
rleLength = foldl (\l (RL _ r) -> l + r) 0 . unRLE

rle :: Eq a => [a] -> RLE a
rle = RLE . map (\(x:l) -> RL x (succ $ length l)) . group

rleV :: Eq a => V.Vector a -> RLEV a
rleV = RLE . VG.unstream . VB.inplace rles VBS.toMax . VG.stream where
  rles (VS.Stream step st) = VS.Stream step' (Nothing, st) where
    step' (m, s) = do
      t <- step s
      case t of
        VS.Yield x s' -> case m of
          Nothing -> return $ VS.Skip (Just (RL x 1), s')
          Just r@(RL y n)
            | x == y -> return $ VS.Skip (Just (RL x $ succ n), s')
            | otherwise -> return $ VS.Yield r (Just (RL x 1), s')
        VS.Skip s' -> return $ VS.Skip (m, s')
        VS.Done -> return $ maybe VS.Done (\r -> VS.Yield r (Nothing, s)) m

sortRLE :: Ord a => RLE a -> RLE a
sortRLE = withRLE $ map (\(RL x r:l) -> RL x (r + rleLength (RLE l))) . groupBy ((==) `on` unRL) . sortOn unRL

trimRLE :: RLE a -> RLE a
trimRLE = withRLE $ filter ((0 /=) . rl)

trimRLEV :: RLEV a -> RLEV a
trimRLEV = withRLE $ V.filter ((0 /=) . rl)

filterRLE :: (a -> Bool) -> RLE a -> RLE a
filterRLE f = withRLE $ filter (f . unRL)

filterRLEV :: (a -> Bool) -> RLEV a -> RLEV a
filterRLEV f = withRLE $ V.filter (f . unRL)

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

intersectChrStr :: PatChar -> ChrStr -> ChrStr
intersectChrStr (PatSet s) t = M.restrictKeys t s
intersectChrStr (PatNot n) t = M.withoutKeys t n
intersectChrStr (PatChr c) t = foldMap (M.singleton c) $ M.lookup c t

allChrs :: PatChar -> ChrStr -> Bool
allChrs p = M.null . intersectChrStr (notChar p)

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
