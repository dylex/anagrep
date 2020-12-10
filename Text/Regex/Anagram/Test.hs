{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Text.Regex.Anagram.Test
  ( testAnagrex
  ) where

import           Control.Arrow (second)
import qualified Data.Bits as B
import           Data.Functor.Identity (runIdentity)
import qualified Data.IntMap.Strict as M
import qualified Data.IntSet as S
import           Data.List (foldl')
import           Data.Ord (comparing)
import qualified Data.Vector as V
import           Numeric.Natural (Natural)

import Text.Regex.Anagram.Types
import Text.Regex.Anagram.Util
import Text.Regex.Anagram.Compile
import Text.Regex.Anagram.Bits

newtype BitVec = BitVec{ unBitVec :: Natural }
  deriving (Eq, B.Bits, FindBits, Show)

instance Semigroup BitVec where
  BitVec x <> BitVec y = BitVec $ x B..|. y
instance Monoid BitVec where
  mempty = BitVec 0

instance Ord BitVec where
  compare (BitVec x) (BitVec y) = comparing B.popCount x y <> compare x y

allBitv :: RLEV a -> BitVec
allBitv = BitVec . allBits . V.length . unRLE

data Matrix a b = Matrix
  { matCols :: !(RLEV (a, BitVec))
  , matRows :: !(RLEV b)
  } deriving (Show)

transpose :: Matrix a b -> Matrix b a
transpose (Matrix (RLE al) (RLE bl)) = Matrix
  (RLE $ V.imap (fmap . tp) bl)
  (RLE $ V.map (fmap fst) al)
  where
  tp i b = (b, V.ifoldl' (\y j v -> if B.testBit (snd $ unRL v) i then B.setBit y j else y) mempty al)

patMatrix :: RLE PatChar -> RLEV Chr -> M.IntMap Int -> Matrix () Chr
patMatrix l ch si = Matrix (fmap ((),) pv) ch where
  pv = withRLE V.fromList $ sortRLE $ fmap vp l
  vp (PatChr c) = maybe mempty B.bit $ M.lookup c si
  vp (PatSet s) = M.foldl' B.setBit   mempty       $ M.restrictKeys si s
  vp (PatNot n) = M.foldl' B.clearBit (allBitv ch) $ M.restrictKeys si n
    -- M.foldl' B.setBit mempty $ M.withoutKeys si n

dropPairs :: Matrix a b -> RLE (Int, RL b) -> Matrix a b
dropPairs m jvrl = m
  { matCols = withRLE (V.map (fmap $ second (jm B..&.))) $ matCols m
  , matRows = withRLE (V.// map (\(RL (j, v) r) -> (j, v{ rl = rl v - r })) (unRLE jvrl)) $ matRows m
  }
  where
  jm = foldl' (\b (RL (j, RL _ jr) r) -> if jr == r then B.clearBit b j else b) (allBitv $ matRows m) $ unRLE jvrl

prioVec :: RL (a, BitVec) -> RL (a, BitVec) -> Ordering
prioVec (RL _      0) (RL _      0) = EQ
prioVec (RL _      0) _             = GT
prioVec _             (RL _      0) = LT
prioVec (RL (_, a) _) (RL (_, b) _) = compare a b

-- |All subsets of a given length (choose p)
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
  | V.null (unRLE $ matCols m) = [m]
  | r == 0 = [m]
  | x == mempty = []
  | otherwise = do
    let m' = m{ matCols = withRLE (V.// [(i, iv{ rl = 0 })]) $ matCols m }
    s <- subsets (rl iv) vjlr
    tryMatrix $ dropPairs m' s
  where
  i = V.minIndexBy prioVec $ unRLE $ matCols m
  iv@(RL (_,x) r) = V.unsafeIndex (unRLE $ matCols m) i
  vjlr = RLE $ map (\j -> let jr = V.unsafeIndex (unRLE $ matRows m) j in RL (j, jr) (rl jr)) $
    findBits x

testReq :: Matrix () Chr -> [RLEV Chr]
testReq m = map matRows $ tryMatrix m

testOpt :: Matrix Chr () -> RLEV Chr -> Bool
testOpt m cv = any (V.all ((0 ==) . rl) . unRLE . matCols) $
  tryMatrix m{ matCols = withRLE (V.zipWith zf $ unRLE $ matCols m) cv }
  where
  zf r r' = r{ rl = rl r' }
    -- | c /= c' || r < r' = error "testOpt mismatch"

takeChars :: PatChar -> RLEV Chr -> RLEV Chr
takeChars p = RLE . V.map fr . unRLE where
  fr r@(RL _ 0) = r
  fr r@(RL c _)
    | f c = r{ rl = 0 }
    | otherwise = r
  f = case p of
    PatChr c -> (c ==)
    PatSet s -> (`S.member` s)
    PatNot n -> (`S.notMember` n)

testPat :: Int -> ChrStr -> AnaPat -> Bool
testPat l s AnaPat{ .. }
  | l < patMin = False
  | Fin l > patMax = False
  | not $ allChrs (patStar patSets) s = False
  | otherwise =
    any (testOpt opts . takeChars (patStar patChars)) $
      testReq reqs
  where
  sv = withRLE V.fromList $ chrStrRLE $ intersectChrStr (runIdentity $ patOpts patSets) s
  si = M.fromAscList $ V.toList $ V.imap (\i (RL c _) -> (c,i)) $ unRLE sv
  reqs = patMatrix (patReqs patChars) sv si
  opts = transpose $ patMatrix (patOpts patChars) sv si

-- |Check if any permutations of a string matches a parsed regular expression.  Always matches the full string.
testAnagrex :: Anagrex -> String -> Bool
testAnagrex (Anagrex l) s = any (testPat (length s) $ chrStr $ map fromEnum s) l
