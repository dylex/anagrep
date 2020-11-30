{-# LANGUAGE RecordWildCards #-}

module Text.Regex.Anagram.Types
  where

import           Data.CaseInsensitive (FoldCase(..))
import qualified Data.IntMap.Strict as M
import qualified Data.IntSet as S
import           Data.Ord (comparing)
import           Data.Semigroup (stimes)

data Inf a
  = Fin !a
  | Inf
  deriving (Eq, Ord, Show)

-- we don't really use this whole instance
instance (Eq a, Num a) => Num (Inf a) where
  Fin a + Fin b = Fin $ a + b
  Inf + _ = Inf
  _ + Inf = Inf
  Fin a * Fin b = Fin $ a * b
  Inf * Fin 0 = Fin 0
  Inf * _ = Inf
  Fin 0 * Inf = Fin 0
  _ * Inf = Inf
  abs (Fin a) = Fin a
  abs Inf = Inf
  signum (Fin a) = Fin (signum a)
  signum Inf = Fin 1
  negate (Fin a) = Fin (negate a)
  negate Inf = error "negate Inf"
  fromInteger = Fin . fromInteger

-- |We use Int for all Chars, mainly to use IntSet.
type Chr = Int
type ChrSet = S.IntSet
-- |A permuted string: a bag of characters mapping character to repeat count.
type ChrStr = M.IntMap Int

-- |Match for a single character.
data PatChar
  = PatChr !Chr -- ^literal single character
  | PatSet ChrSet -- ^one of a set "[a-z]"
  | PatNot ChrSet -- ^not one of a set "[^a-z]"
  deriving (Eq, Show)

instance Semigroup PatChar where
  PatSet s <> x | S.null s = x -- opt
  PatSet s <> PatChr c = PatSet (S.insert c s)
  PatSet s <> PatSet t = PatSet (S.union s t)
  PatSet s <> PatNot n = PatNot (S.difference n s)
  PatChr c <> PatChr d
    | c == d = PatChr c
    | otherwise = PatSet (S.fromList [c,d])
  PatChr c <> PatNot n = PatNot (S.delete c n)
  PatNot n <> PatNot m = PatNot (S.intersection n m)
  a <> b = b <> a

instance Monoid PatChar where
  mempty = PatSet S.empty

instance Ord PatChar where
  compare (PatChr c1) (PatChr c2) = compare c1 c2
  compare (PatSet s1) (PatSet s2) = comparing S.size s1 s2 <> compare s1 s2
  compare (PatNot s1) (PatNot s2) = comparing S.size s2 s1 <> compare s1 s2
  compare (PatChr _) _ = LT
  compare _ (PatChr _) = GT
  compare (PatSet _) (PatNot _) = LT
  compare (PatNot _) (PatSet _) = GT

data Pat = Pat
  { patChars :: ChrStr -- ^required fixed chars (grouped 'PatChr')
  , patSets :: [PatChar] -- ^other requried sets (no 'PatChr')
  , patOpts :: [PatChar] -- ^optional chars (x?)
  , patStars :: PatChar -- ^extra chars (x*)
  , patMin :: Int -- ^minimum length (optimization)
  , patMax :: Inf Int -- ^maximum length (optimization)
  } deriving (Show)

instance Semigroup Pat where
  Pat a1 l1 o1 e1 i1 j1 <> Pat a2 l2 o2 e2 i2 j2 =
    Pat (M.unionWith (+) a1 a2) (l1 ++ l2) (o1 ++ o2) (e1 <> e2) (i1 + i2) (j1 + j2)
  stimes n (Pat a l o e i j) = Pat (fmap (n'*) a) (stimes n l) (stimes n o) e (n'*i) (Fin n'*j)
    where n' = fromIntegral n

instance Monoid Pat where
  mempty = Pat M.empty [] [] mempty 0 0

-- |A processed regular expression pattern to match anagrams.
-- Represented as an (expanded) list of alternative 'Pat's.
newtype Anagrex = Anagrex [Pat]
  deriving (Show)


foldCaseChr :: Chr -> Chr
foldCaseChr c = fromEnum (foldCase (toEnum c :: Char))

instance FoldCase PatChar where
  foldCase (PatChr c) = PatChr (foldCaseChr c)
  foldCase (PatSet s) = PatSet (S.map foldCaseChr s)
  foldCase (PatNot s) = PatNot (S.map foldCaseChr s)

instance FoldCase Pat where
  foldCase p@Pat{..} = p
    { patChars = M.mapKeysWith (+) foldCaseChr patChars
    , patSets = map foldCase patSets
    , patOpts = map foldCase patOpts
    , patStars = foldCase patStars
    }

instance FoldCase Anagrex where
  foldCase (Anagrex l) = Anagrex (map foldCase l)

