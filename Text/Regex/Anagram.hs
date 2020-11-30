{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Text.Regex.Anagram
  ( Anagrex
  , parseAnagrex
  , testAnagrex
  , testAnagrexCI
  ) where

import           Control.Monad (MonadPlus, mfilter, foldM)
import           Data.CaseInsensitive (CI, FoldCase(..), foldedCase)
import           Data.Foldable (fold, foldlM)
import qualified Data.IntMap.Strict as M
import qualified Data.IntSet as S
import           Data.Maybe (isJust, mapMaybe)
import           Data.Ord (comparing)
import           Data.Semigroup (stimes)
import qualified Data.Set as Set
import qualified Text.Regex.TDFA.Pattern as R
import qualified Text.Regex.TDFA.ReadRegex as R

foldMapM :: (Monad m, Monoid b) => (a -> m b) -> [a] -> m b
-- foldMapM f = fmap fold . mapM f
foldMapM f = foldlM (\b a -> (b <>) <$> f a) mempty

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
-- concatMapM = foldMapM
concatMapM f = fmap concat . mapM f

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

chrSet :: Set.Set Char -> ChrSet
chrSet = S.fromAscList . map fromEnum . Set.toAscList

chrStr :: String -> ChrStr
chrStr = M.fromListWith (+) . map ((, 1) . fromEnum)

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

_differenceChar :: PatChar -> PatChar -> PatChar
_differenceChar a b = intersectChar a (notChar b)

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

-- parsing

unChars :: ChrStr -> [PatChar]
unChars = concatMap (uncurry $ flip replicate . PatChr) . M.toList where

charPat :: PatChar -> Pat
charPat (PatChr c) = mempty{ patChars = M.fromList [(c,1)], patMin = 1, patMax = 1 }
charPat p = mempty{ patSets = [p], patMin = 1, patMax = 1 }

makeChar :: R.Pattern -> Maybe PatChar
makeChar R.PDot{} = return $ PatNot S.empty
makeChar R.PChar{ R.getPatternChar = c } = return $ PatChr $ fromEnum c
makeChar R.PEscape{ R.getPatternChar = c }
  | c `notElem` "`'<>bB" = return $ PatChr $ fromEnum c
-- TODO: use R.decodePatternSet
makeChar R.PAny{ R.getPatternSet = R.PatternSet (Just s) Nothing Nothing Nothing } =
  return $ PatSet $ chrSet s
makeChar R.PAnyNot{ R.getPatternSet = R.PatternSet (Just s) Nothing Nothing Nothing } =
  return $ PatNot $ chrSet s
makeChar _ = Nothing

starPat :: Pat -> PatChar
starPat Pat{..} = PatSet (M.keysSet patChars) <> fold patSets <> fold patOpts <> patStars

questPat :: Pat -> [PatChar]
questPat Pat{..} = unChars patChars ++ patSets ++ patOpts

makePattern :: R.Pattern -> Maybe Pat
makePattern (R.PGroup _ r) = makePattern r
makePattern (R.PNonCapture r) = makePattern r
makePattern (R.POr [r]) = makePattern r
makePattern (R.PConcat l) = foldMapM makePattern l
makePattern (R.PQuest r) = do
  p <- makePattern r
  return mempty{ patOpts = questPat p, patStars = patStars p, patMax = patMax p }
makePattern (R.PPlus r) = do
  p <- makePattern r
  return p{ patStars = starPat p, patMax = Inf * patMax p }
makePattern (R.PStar _ r) = do
  p <- makePattern r
  return mempty{ patStars = starPat p, patMax = Inf * patMax p }
makePattern (R.PBound i j' r) = do
  p <- makePattern r
  let ip = stimes i p
  return $ maybe
    ip{ patStars = starPat p, patMax = Inf * patMax p }
    (\j -> ip{ patOpts = patOpts ip ++ stimes (j - i) (questPat p), patMax = patMax ip + Fin (j - i) * patMax p })
    j'
makePattern R.PEmpty = return mempty
makePattern r = charPat <$> makeChar r

makeAlts :: R.Pattern -> Maybe [Pat]
makeAlts (R.PGroup _ r) = makeAlts r
makeAlts (R.PNonCapture r) = makeAlts r
makeAlts (R.POr o) = concatMapM makeAlts o
makeAlts (R.PConcat c) = cross <$> mapM makeAlts c where
  cross [] = [mempty]
  cross (l:r) = do
    a <- l
    b <- cross r
    return (a <> b)
makeAlts r = return <$> makePattern r

-- optimization

-- |Remove everything in patStars from patOpts
filterStar :: Pat -> Pat
filterStar p@Pat{..} = p
  { patOpts = mapMaybe (mfilter (not . nullChar) . Just . intersectChar (notChar patStars)) patOpts
  }

optimizePat :: Pat -> Pat
optimizePat = filterStar

optimizeAlts :: [Pat] -> [Pat]
optimizeAlts = map optimizePat

-- |Parse a string as a regular expression for matching anagrams, returning 'Left' error for invalid or unsupported regular expressions.  (Uses 'R.parseRegex'.)
parseAnagrex :: String -> Either String Anagrex
parseAnagrex r = case R.parseRegex r of
  Left e -> Left (show e)
  Right (p, _) -> maybe (Left "regexp contains features not supported for anagrams")
    (Right . Anagrex . optimizeAlts) $ makeAlts $ R.dfsPattern R.simplify' p

-- testing

maybePred :: Int -> Maybe Int
maybePred i
  | 1 < i = Just $ pred i
  | otherwise = Nothing

subtractStr :: ChrStr -> ChrStr -> ChrStr
subtractStr = M.mergeWithKey
  (\_ i j -> if i == j then Nothing else Just (i - j))
  id (fmap negate)

subtractStr' :: MonadPlus m => ChrStr -> ChrStr -> m ChrStr
subtractStr' a b = mfilter (all (0 <)) $ return $ subtractStr a b

subtractEach :: ChrStr -> M.IntMap a -> [ChrStr]
subtractEach m = map (\c -> M.update maybePred c m) . M.keys

takeChar :: PatChar -> ChrStr -> [ChrStr]
takeChar (PatChr c) m
  | isJust j = [m']
  | otherwise = [] where
  (j, m') = M.updateLookupWithKey (\_ -> maybePred) c m
takeChar (PatSet s) m = subtractEach m $ M.restrictKeys m s
takeChar (PatNot s) m = subtractEach m $ M.withoutKeys  m s

takeChars :: PatChar -> M.IntMap a -> M.IntMap a
takeChars (PatChr c) m = M.delete c m
takeChars (PatSet s) m = M.withoutKeys m s
takeChars (PatNot s) m = M.restrictKeys m s

testPat :: Int -> ChrStr -> Pat -> Bool
testPat l m0 Pat{..}
  | l < patMin = False
  | Fin l > patMax = False
  | otherwise = any M.null $ do
  ma <- subtractStr' m0 patChars
  ml <- foldM (       flip takeChar)     ma patSets
  mo <- foldM (\m c -> m : takeChar c m) ml patOpts
  return $ takeChars patStars mo

-- |Check if any permutations of a string matches a parsed regular expression.  Always matches the full string.
testAnagrex :: Anagrex -> String -> Bool
testAnagrex (Anagrex l) s = any (testPat (length s) (chrStr s)) l

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

-- |Test a case-insensitive pattern against a case-insensitive string.  You can also directly use 'foldCase' on both the pattern and the string to test.
testAnagrexCI :: CI Anagrex -> CI String -> Bool
testAnagrexCI p = testAnagrex (foldedCase p) . foldedCase
