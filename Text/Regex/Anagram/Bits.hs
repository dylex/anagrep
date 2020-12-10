{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MagicHash #-}

module Text.Regex.Anagram.Bits
  where

import           Data.Bits
import           Data.Int
import           Data.Word
import           Numeric.Natural (Natural)

#ifdef VERSION_ghc_prim
import qualified GHC.Integer.GMP.Internals as GHC
import qualified GHC.Natural as GHC
import qualified GHC.Types as GHC
#include "MachDeps.h"
#endif

-- |value with all lower n bits set
allBits :: (Enum b, Bits b) => Int -> b
allBits = pred . bit

class Bits b => FindBits b where
  -- |list of all set bits in a value
  findBits :: b -> [Int]
  default findBits :: FiniteBits b => b -> [Int]
  findBits w
    | i == finiteBitSize w = []
    | otherwise = i : findBits (clearBit w i)
    where i = countTrailingZeros w

instance FindBits Int
instance FindBits Int8
instance FindBits Int16
instance FindBits Int32
instance FindBits Int64
instance FindBits Word
instance FindBits Word8
instance FindBits Word16
instance FindBits Word32
instance FindBits Word64

instance FindBits Natural where
#ifdef VERSION_ghc_prim
  findBits (GHC.NatS# w) = findBits (GHC.W# w)
  findBits (GHC.NatJ# b) = concatMap fbi [0..pred (GHC.I# (GHC.sizeofBigNat# b))]
    where
    fbi i@(GHC.I# i#) = let w = GHC.W# (GHC.indexBigNat# b i#) in map (i*finiteBitSize w+) $ findBits w
#else
  findBits = fb 0 . unBitVec where
    fb i x
      | x == B.zeroBits = []
      | B.testBit x i = i : fb (succ i) (B.clearBit x i)
      | otherwise = fb (succ i) x
#endif
