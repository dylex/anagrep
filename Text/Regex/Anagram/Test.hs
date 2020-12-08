{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Text.Regex.Anagram.Test
  ( testAnagrex
  ) where

import           Control.Arrow (second)
import qualified Data.Bits as B
import qualified Data.IntMap.Strict as M
import qualified Data.IntSet as S
import           Data.List (sort, foldl')
import           Data.Ord (comparing)
import qualified Data.Vector as V
import           Numeric.Natural (Natural)

import Text.Regex.Anagram.Types
import Text.Regex.Anagram.Util
import Text.Regex.Anagram.Compile

newtype BitVec = BitVec{ unBitVec :: Natural }
  deriving (Eq, B.Bits, Show)

instance Semigroup BitVec where
  BitVec x <> BitVec y = BitVec $ x B..|. y
instance Monoid BitVec where
  mempty = BitVec 0

instance Ord BitVec where
  compare (BitVec x) (BitVec y) = comparing B.popCount x y <> compare x y

allBits :: RLEV a -> BitVec
allBits = BitVec . pred . B.bit . V.length . unRLE

findBits :: BitVec -> [Int]
findBits = fb 0 . unBitVec where
  fb i x
    | x == B.zeroBits = []
    | B.testBit x i = i : fb (succ i) (B.clearBit x i)
    | otherwise = fb (succ i) x

data Matrix a b = Matrix
  { matCols :: !(RLEV (a, BitVec))
  , matRows :: !(RLEV b)
  } deriving (Show)

transpose :: Matrix a b -> Matrix b a
transpose (Matrix (RLE al) (RLE bl)) = Matrix
  (RLE $ V.imap (\i -> fmap (tp i)) bl)
  (RLE $ V.map (fmap fst) al)
  where
  tp i b = (b, V.ifoldl' (\y j v -> if B.testBit (snd $ unRL v) i then B.setBit y j else y) mempty al)

patMatrix :: [PatChar] -> RLEV Chr -> Matrix () Chr
patMatrix l ch = Matrix (RLE $ V.map (fmap ((),)) pv) ch where
  si = M.fromAscList $ V.toList $ V.imap (\i (RL c _) -> (c,i)) $ unRLE ch
  pv = V.fromList $ unRLE $ rle $ sort $ map vp l
  vp (PatChr c) = maybe mempty B.bit $ M.lookup c si
  vp (PatSet s) = M.foldl' B.setBit mempty $ M.restrictKeys si s
  vp (PatNot n) =
    -- M.foldl' B.setBit mempty $ M.withoutKeys si n
    M.foldl' B.clearBit (allBits ch) $ M.restrictKeys si n

dropPairs :: Matrix a b -> RLE (Int, RL b) -> Matrix a b
dropPairs m jvrl = m
  { matCols = RLE $ V.map (fmap $ second (jm B..&.)) $ unRLE $ matCols m
  , matRows = RLE $ unRLE (matRows m) V.// map (\(RL (j, v) r) -> (j, v{ rl = rl v - r })) (unRLE jvrl)
  }
  where
  jm = foldl' (\b (RL (j, RL _ jr) r) -> if jr == r then B.clearBit b j else b) (allBits $ matRows m) $ unRLE jvrl

prioVec :: RL (a, BitVec) -> RL (a, BitVec) -> Ordering
prioVec (RL _      0) (RL _      0) = EQ
prioVec (RL _      0) _             = GT
prioVec _             (RL _      0) = LT
prioVec (RL (_, a) _) (RL (_, b) _) = compare a b

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

tryMatrix :: (Show a, Show b) => Matrix a b -> [Matrix a b]
tryMatrix m
  | r == 0 = [m]
  | x == mempty = []
  | otherwise = do
    let m' = m{ matCols = RLE $ (unRLE $ matCols m) V.// [(i, iv{ rl = 0 })] }
    s <- subsets (rl iv) vjlr
    tryMatrix $ dropPairs m' s
  where
  i = V.minIndexBy prioVec $ unRLE $ matCols m
  iv@(RL (_,x) r) = V.unsafeIndex (unRLE $ matCols m) i
  vjlr = RLE $ map (\j -> let jr = V.unsafeIndex (unRLE $ matRows m) j in RL (j, jr) (rl jr)) $
    findBits x

testReq :: [PatChar] -> RLEV Chr -> [RLEV Chr]
testReq [] s = [s]
testReq l s
  | V.null (unRLE s) = []
  | otherwise = map matRows $
    tryMatrix $ patMatrix l s

testOpt :: [PatChar] -> RLEV Chr -> Bool
testOpt _ (RLE s) | V.null s = True
testOpt [] _ = False
testOpt l s = any (V.all ((0 ==) . rl) . unRLE . matCols) $
  tryMatrix $ transpose $ patMatrix l s

takeChars :: PatChar -> RLEV Chr -> RLEV Chr
takeChars p = RLE . V.filter (\(RL c r) -> r /= 0 && fp c) . unRLE where
  fp = f p
  f (PatChr c) = (c /=)
  f (PatSet s) = (`S.notMember` s)
  f (PatNot n) = (`S.member` n)

testPat :: Int -> ChrStr -> AnaPat -> Bool
testPat l s AnaPat{ patUncompiled = p, .. }
  | l < patMin = False
  | Fin l > patMax = False
  | otherwise =
    any (testOpt (patOpts p) . takeChars (patStar p)) $
      testReq (patReqs p) $ RLE $ V.fromList $ unRLE $ chrStrRLE s

-- |Check if any permutations of a string matches a parsed regular expression.  Always matches the full string.
testAnagrex :: Anagrex -> String -> Bool
testAnagrex (Anagrex l) s = any (testPat (length s) $ chrStr $ map fromEnum s) l
