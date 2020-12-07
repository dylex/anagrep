{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Text.Regex.Anagram.Test
  ( testAnagrex
  ) where

import           Control.Monad (when)
import           Control.Monad.ST (ST)
import qualified Data.Bits as B
import qualified Data.IntMap.Strict as M
import qualified Data.IntSet as S
import           Data.List (sort)
import           Data.Maybe (mapMaybe)
import           Data.Ord (comparing)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import           Numeric.Natural (Natural)

import Text.Regex.Anagram.Types
import Text.Regex.Anagram.Util
import Text.Regex.Anagram.Compile

import Debug.Trace

newtype BitVec = BitVec Natural
  deriving (Eq, B.Bits, Show)

instance Semigroup BitVec where
  (<>) = (B..|.)
instance Monoid BitVec where
  mempty = BitVec 0

instance Ord BitVec where
  compare (BitVec x) (BitVec y) = comparing B.popCount x y <> compare x y

findBits :: B.Bits a => a -> [Int]
findBits = fb 0 where
  fb i x
    | x == B.zeroBits = []
    | B.testBit x i = i : fb (succ i) (B.clearBit x i)
    | otherwise = fb (succ i) x

data MatVec a = MatVec
  { matHdr :: !a
  , matRep :: !Int
  , matVec :: !BitVec
  , matSum :: !Int
  } deriving (Eq, Show)

type Matrix a = V.Vector (MatVec a)
type MMatrix s a = V.MVector s (MatVec a)

data Matrix' a b = Matrix'
  { matrix  :: !(Matrix a)
  , matrix' :: !(Matrix b)
  } deriving (Show)

mkMatVec :: RL a -> BitVec -> MatVec a
mkMatVec (RL h r) x = MatVec h r x (B.popCount x)

matrixStr :: Matrix Chr -> RLE Chr
matrixStr = RLE . mapMaybe (\v -> guard' (matRep v > 0) $ RL (matHdr v) (matRep v)) . V.toList

transpose :: Matrix a -> RLEV b -> Matrix b
transpose m = V.imap (\i r ->
  let x = V.ifoldl' (\y n v -> if B.testBit (matVec v) i then B.setBit y n else y) mempty m in
  mkMatVec r x) . unRLE

mkMatrix' :: Matrix a -> RLEV b -> Matrix' a b
mkMatrix' m = Matrix' m . transpose m

transpose' :: Matrix' a b -> Matrix' b a
transpose' (Matrix' a b) = Matrix' b a

patMatrix :: [PatChar] -> RLE Chr -> Matrix' () Chr
patMatrix l sr = mkMatrix' mb (RLE ch) where
  ch = V.fromList $ unRLE sr
  si = M.fromAscList $ map (\(i,RL c _) -> (c,i)) $ V.toList $ V.indexed ch
  mb = V.fromList $ map (\(RL x r) -> mkMatVec (RL () r) x) $ unRLE $ rle $ sort $ map vp l
  vp (PatChr c) = maybe mempty B.bit $ M.lookup c si
  vp (PatSet s) = M.foldl' B.setBit mempty $ M.restrictKeys si s
  vp (PatNot n) =
    -- M.foldl' B.setBit mempty $ M.withoutKeys si n
    M.foldl' B.clearBit (BitVec $ pred $ B.bit $ V.length ch) $ M.restrictKeys si n

decrRow :: MMatrix s a -> Int -> [Int] -> ST s ()
decrRow m j b = mapM_ decr b where
  decr = MV.unsafeModify m $ \v -> v{ matVec = B.clearBit (matVec v) j, matSum = pred (matSum v) }

decrPair :: MMatrix s a -> Int -> Int -> MatVec b -> [Int] -> Int -> ST s ()
decrPair m i j jv jb r = do
  when (matRep jv == r) $ decrRow m j jb
  MV.unsafeModify m (\v -> v{ matRep = matRep v - r }) i

dropPairs' :: Matrix' a b -> Int -> RLE (Int, MatVec b) -> Matrix' a b
dropPairs' (Matrix' m n) i jvrl = Matrix'
  (V.modify dropi m)
  (V.modify dropj n)
  where
  dropi mm =
    mapM_ (\(RL (j, v) r) -> decrPair mm i j v (findBits $ matVec v) r) $ unRLE jvrl
  dropj nm =
    mapM_ (\(RL (j, v) r) -> MV.unsafeWrite nm j $ v{ matRep = matRep v - r, matVec = B.clearBit (matVec v) i, matSum = pred (matSum v) }) $ unRLE jvrl

prioVec :: MatVec a -> MatVec a -> Ordering
prioVec MatVec{ matRep = 0 } MatVec{ matRep = 0 } = EQ
prioVec MatVec{ matRep = 0 } _                    = GT
prioVec _                    MatVec{ matRep = 0 } = LT
prioVec MatVec{ matSum = a } MatVec{ matSum = b } = compare a b

subsets :: Int -> RLE a -> [RLE a]
subsets size list = map RLE $ ss size (rleLength list) (unRLE list) where
  ss 0 _ _ = [[]]
  ss n s l = case compare n s of
    GT -> []
    EQ -> [l]
    LT -> do
      let ~(RL x r:m) = l
      i <- [max 0 (n+r-s)..min n r]
      (if i > 0 then (RL x i :) else id) <$> ss (n-i) (s-r) m

tryMatrix :: (Show a, Show b) => Matrix' a b -> [Matrix' a b]
tryMatrix m
  | matRep v == 0 = [m]
  | matSum v == 0 = []
  | otherwise = do
    s <- subsets (matRep v) vjlr
    tryMatrix $ dropPairs' m i s
  where
  i = V.minIndexBy prioVec (matrix m)
  v = V.unsafeIndex (matrix m) i
  vjlr = RLE $ map (\j -> let jv = V.unsafeIndex (matrix' m) j in RL (j, jv) (matRep jv)) $
    findBits $ matVec v

testReq :: [PatChar] -> RLE Chr -> [RLE Chr]
testReq [] s = [s]
testReq _ (RLE []) = []
testReq l s = map (matrixStr . matrix') $
  tryMatrix $ patMatrix l s

testOpt :: [PatChar] -> RLE Chr -> Bool
testOpt _ (RLE []) = True
testOpt [] _ = False
testOpt l s = any (V.all ((0 ==) . matRep) . matrix) $
  tryMatrix $ transpose' $ patMatrix l s

takeChars :: PatChar -> RLE Chr -> RLE Chr
takeChars (PatChr c) = filterRLE (c /=)
takeChars (PatSet s) = filterRLE (`S.notMember` s)
takeChars (PatNot n) = filterRLE (`S.member` n)

testPat :: Int -> ChrStr -> AnaPat -> Bool
testPat l s AnaPat{ patUncompiled = p, .. }
  | l < patMin = False
  | Fin l > patMax = False
  | otherwise =
    any (testOpt (patOpts p) . takeChars (patStar p)) $
      testReq (patReqs p) $ chrStrRLE s

-- |Check if any permutations of a string matches a parsed regular expression.  Always matches the full string.
testAnagrex :: Anagrex -> String -> Bool
testAnagrex (Anagrex l) s = any (testPat (length s) $ chrStr $ map fromEnum s) l
