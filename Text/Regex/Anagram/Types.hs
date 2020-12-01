{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Text.Regex.Anagram.Types
  where

import           Control.Arrow (first)
import           Data.CaseInsensitive (FoldCase(..))
import qualified Data.IntMap.Strict as M
import qualified Data.IntSet as S
import           Data.Ord (comparing)
import           Data.Semigroup (stimes)
import qualified Data.Vector as V

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
  compare (PatSet _) (PatNot _) = LT
  compare _ _ = GT

-- |The parsed characters from a regex pattern
data PatCharsOf f = PatChars
  { patReqs :: f PatChar -- ^requried chars
  , patOpts :: f PatChar -- ^optional chars (x?)
  , patStar :: PatChar -- ^extra chars (x*)
  }

type PatChars = PatCharsOf []

deriving instance Show PatChars

instance Semigroup PatChars where
  PatChars l1 o1 e1 <> PatChars l2 o2 e2 =
    PatChars (l1 <> l2) (o1 <> o2) (e1 <> e2)
  stimes n (PatChars l o e) = PatChars (stimes n l) (stimes n o) e

instance Monoid PatChars where
  mempty = PatChars mempty mempty mempty

newtype Graph a = Graph{ unGraph :: V.Vector (a, S.IntSet) }
  deriving (Show)

deriving instance Show (PatCharsOf Graph)

-- |Compiled matching pattern
data AnaPat = AnaPat
  { patFixed :: ChrStr -- ^required fixed chars (grouped 'PatChr')
  , patChars :: PatCharsOf Graph
  , patMin :: Int -- ^minimum length
  , patMax :: Inf Int -- ^maximum length
  } deriving (Show)

-- |A processed regular expression pattern to match anagrams.
-- Represented as an (expanded) list of alternative 'AnaPat's.
newtype Anagrex = Anagrex [AnaPat]
  deriving (Show)


foldCaseChr :: Chr -> Chr
foldCaseChr c = fromEnum (foldCase (toEnum c :: Char))

instance FoldCase PatChar where
  foldCase (PatChr c) = PatChr (foldCaseChr c)
  foldCase (PatSet s) = PatSet (S.map foldCaseChr s)
  foldCase (PatNot s) = PatNot (S.map foldCaseChr s)

instance Functor f => FoldCase (PatCharsOf f) where
  foldCase p@PatChars{..} = p
    { patReqs = fmap foldCase patReqs
    , patOpts = fmap foldCase patOpts
    , patStar = foldCase patStar
    }

-- XXX really should be re-made
instance Functor Graph where
  fmap f (Graph v) = Graph $ fmap (first f) v

instance FoldCase AnaPat where
  foldCase p@AnaPat{..} = p
    { patFixed = M.mapKeysWith (+) foldCaseChr patFixed
    , patChars = foldCase patChars -- XXX
    }

instance FoldCase Anagrex where
  foldCase (Anagrex l) = Anagrex (map foldCase l)

