{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Text.Regex.Anagram.Test
  ( testAnagrex
  ) where

import           Control.Arrow (second)
import           Control.Monad (join)
import qualified Data.Bits as B
import           Data.Functor.Identity (runIdentity)
import qualified Data.IntMap.Strict as M
import           Data.List (mapAccumL)
import           Data.Maybe (isJust, fromJust)
import           Data.Ord (comparing)
import qualified Data.Vector as V
import           Numeric.Natural (Natural)

import Text.Regex.Anagram.Types
import Text.Regex.Anagram.Util
import Text.Regex.Anagram.Compile
import Text.Regex.Anagram.Bits

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

data Pat
  = Req !BitVec
  | Opt
  | Star
  deriving (Eq, Ord, Show)

-- |just a lens
class HasBitVec a where
  getBitVec :: a -> BitVec
  mapBitVec :: (BitVec -> BitVec) -> a -> a

instance HasBitVec BitVec where
  getBitVec = id
  mapBitVec = id

instance HasBitVec (a, BitVec) where
  getBitVec = snd
  mapBitVec = second

instance HasBitVec Pat where
  getBitVec ~(Req a) = a
  mapBitVec f (Req a) = Req (f a)
  mapBitVec _ p = p

instance HasBitVec a => HasBitVec (RL a) where
  getBitVec = getBitVec . unRL
  mapBitVec f = fmap (mapBitVec f)

transpose' :: (HasBitVec a, HasBitVec b) => RLEV a -> RLEV b -> RLEV b
transpose' (RLE al) (RLE bl) =
  (RLE $ V.imap (fmap . tp) bl)
  where
  tp i = mapBitVec $ const $ V.ifoldl' (\y j v -> if B.testBit (getBitVec v) i then B.setBit y j else y) mempty al

data Matrix a b = Matrix
  { matCols :: !(RLEV a)
  , matRows :: !(RLEV b)
  } deriving (Show)

transpose :: Matrix a b -> Matrix b a
transpose (Matrix a b) = Matrix b a

data PatMatrix = PatMatrix
  { patMatrix :: !(Matrix BitVec Pat)
  , patMatReq :: !(Maybe Int)
  } deriving (Show)

initMatrix :: PatCharsOf RLE -> ChrStr -> Maybe PatMatrix
initMatrix PatChars{..} cs =
  guard' (all ((mempty /=) . unRL) $ unRLE reqv) $
    PatMatrix (Matrix (transpose' pv $ mempty <$ cv) (fmap fst pv)) (Just $ length $ unRLE reqs)
  where
  cv = withRLE V.fromList $ chrStrRLE cs
  si = M.fromAscList $ V.toList $ V.imap (\i (RL c _) -> (c,i)) $ unRLE cv
  pv = withRLE V.fromList $ reqs <> opts <> stars
  reqv =                         fmap vp patReqs
  optv = filterRLE (mempty /=) $ fmap vp patOpts
  reqs = fmap (join ((,) . Req)) $ sortRLE reqv
  opts = fmap       ((,)   Opt)  $ sortRLE optv
  stars
    | nullChar patStar || vps == mempty = RLE []
    | otherwise = RLE [RL (Star, vps) (B.unsafeShiftR maxBound 1)]
    where vps = vp patStar
  vp (PatChr c) = maybe mempty B.bit $ M.lookup c si
  vp (PatSet s) = M.foldl' B.setBit   mempty       $ M.restrictKeys si s
  vp (PatNot n) = M.foldl' B.clearBit (allBitv cv) $ M.restrictKeys si n
    -- M.foldl' B.setBit mempty $ M.withoutKeys si n

decrRows :: (HasBitVec a, HasBitVec b) => Int -> Matrix a b -> RLE (Int, RL b) -> Matrix a b
decrRows i (Matrix cm rm) l = Matrix
  { matCols = withRLE (V.map (mapBitVec (m B..&.))) cm
  , matRows = withRLE (V.// u) rm
  }
  where
  (m, u) = mapAccumL (\x (RL (j, RL jp jr) r) -> if jr == r
      then (B.clearBit x j, (j, RL (mapBitVec (const mempty)   jp) 0))
      else (           x,   (j, RL (mapBitVec (`B.clearBit` i) jp) (jr - r))))
    (allBitv rm) $ unRLE l

tryCol :: (HasBitVec a, HasBitVec b) => Int -> RL a -> Matrix a b -> [Matrix a b]
tryCol i iv m = map (decrRows i m')
  $ subsets (rl iv)
    $ RLE $ map (\j ->
      let jr = V.unsafeIndex (unRLE $ matRows m) j in
      RL (j, jr) (rl jr))
    $ findBits $ getBitVec iv
  where
  m' = m{ matCols = withRLE (V.// [(i, iv{ rl = 0 })]) $ matCols m }

tryPat :: Int -> RL Pat    -> PatMatrix -> [PatMatrix]
tryChr :: Int -> RL BitVec -> PatMatrix -> [PatMatrix]
tryPat i iv pm = map (\m -> pm{ patMatrix = transpose m }) $ tryCol i iv $ transpose $ patMatrix pm
tryChr i iv pm = map (\m -> pm{ patMatrix = m })           $ tryCol i iv             $ patMatrix pm

prio :: HasBitVec a => RL a -> RL a -> Ordering
prio (RL _ 0) (RL _ 0) = EQ
prio (RL _ 0) _        = GT
prio _        (RL _ 0) = LT
prio (RL a r) (RL b s) = comparing (B.popCount . getBitVec) a b <> compare s r

doneReq :: PatMatrix -> [PatMatrix]
doneReq (PatMatrix m@(Matrix cm pm) ~(Just pr))
  | RL Star _ <- V.last (unRLE pm) = next j $ Matrix
    (withRLE (V.map ts) cm)
    (withRLE V.init pm)
  | otherwise = next (succ j) m
  where
  next j' m' = (if pr >= j' then return else tryMatrix) $ PatMatrix m' Nothing
  ts v@(RL x _)
    | B.testBit x j = RL mempty 0
    | otherwise = v
  j = pred $ V.length $ unRLE pm

tryMatrix :: PatMatrix -> [PatMatrix]
tryMatrix m@(PatMatrix (Matrix cm pm) pr)
  | isJust pr && (pr == Just 0 || jr == 0) = doneReq m
  | isJust pr && (B.popCount y <= 1 || B.popCount x > 1) =
    tryMatrix =<< tryPat j jv m
  | isJust pr && ir == 0 = []
  |              ir == 0 = [m]
  | x == mempty = []
  | otherwise =
    tryMatrix =<< tryChr i iv m
  where
  i = V.minIndexBy prio $                        unRLE cm
  j = V.minIndexBy prio $ V.take (fromJust pr) $ unRLE pm
  iv@(RL       x  ir) = V.unsafeIndex (unRLE cm) i
  jv@(RL ~(Req y) jr) = V.unsafeIndex (unRLE pm) j

testPat :: Int -> ChrStr -> AnaPat -> Bool
testPat l s AnaPat{ .. }
  | l < patMin = False
  | Fin l > patMax = False
  | not $ allChrs (patStar patSets) s = False
  | M.null s' = null $ unRLE $ patReqs patChars
  | otherwise = maybe False
    (any (V.all ((0 ==) . rl) . unRLE . matCols . patMatrix)
      . tryMatrix) $ initMatrix patChars s'
  where
  s' = intersectChrStr (runIdentity $ patOpts patSets) s

-- |Check if any permutations of a string matches a parsed regular expression.  Always matches the full string.
testAnagrex :: Anagrex -> String -> Bool
testAnagrex (Anagrex l) s = any (testPat (length s) $ chrStr $ map fromEnum s) l
