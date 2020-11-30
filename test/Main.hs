import           Control.Monad (forM_)
import           Data.Either (isRight)
import           Test.Hspec
import qualified Test.QuickCheck as Q

import Text.Regex.Anagram

tests :: [(String, [String], [String])]
tests =
  [ ("a*", ["","a","aa","aaa","aaaaaaaaaaaaaa"],
      ["b","ab","bad"])
  , ("a+b", ["ba","ab","baaaa","aaba"],
      ["","bab","abc","b"])
  , ("a[ab]+b", ["baa","aab","aba","abb","bba","bab","aaaba","bbba"],
      ["","ab","aa","aaa","b","xabb"])
  , ("a[ab]*b?", ["baa","aab","aba","abb","bba","bab","aaaba","bbba","aa","ab","aaa","a"],
      ["","bbb","b","abc"])
  , ("ba{2,}", ["aba","aaab","baaaa","aaaaabaaa"],
      ["a","ab","baac"])
  , ("[^a]", ["b","c","d"],
      ["","a","bb","ba"])
  , ("[^a-d]*", ["ee","xyz",""],
      ["a","xa","fce"])
  ]

totals :: [(String, [String])]
totals =
  [ ("()", [""])
  , ("abc", ["abc","acb","bac","bca","cab","cba"])
  , ("[a-d]", ["a","b","c","d"])
  , ("[ab][bc]", ["ab","ac","bb","bc"])
  , ("a[ab]b", ["baa","aab","aba","abb","bba","bab"])
  , ("a[ab]?b?", ["a","ab","aa","baa","aab","aba","abb","bba","bab"])
  , ("(a|b)c", ["ac","ca","bc","cb"])
  , ("(ab|cd)", ["ab","ba","cd","dc"])
  , ("(a|b(b?|c))", ["a","b","bb","bc","cb"])
  , ("a{2,4}", ["aa","aaa","aaaa"])
  ]

props :: [(String, String, Anagrex -> Q.Property)]
props =
  [ (".*", "match everything", \p -> Q.property $
      testAnagrex p)
  , (".+", "match anything", \p -> Q.property $
      \s -> testAnagrex p s Q.=/= null s)
  , (".*x", "match anything with an x", \p -> Q.property $
      \s -> testAnagrex p s Q.=== ('x' `elem` s))
  , ("a.*x", "match anything with an a and x", \p -> Q.property $
      \s -> testAnagrex p s Q.=== ('x' `elem` s && 'a' `elem` s))
  , (".", "match single chars", \p -> Q.property $
      \s -> testAnagrex p s Q.=== (length s == 1))
  , (".{2}", "match two chars", \p -> Q.property $
      \s -> testAnagrex p s Q.=== (length s == 2))
  ]

parse :: String -> (Anagrex -> Spec) -> Spec
parse r t = describe ("pattern " ++ show r) $ do
  let pp = parseAnagrex r
      Right p = pp
  it "should parse" $ const () <$> pp `shouldSatisfy` isRight
  t p

main :: IO ()
main = hspec $ describe "Text.Regex.Anagram" $ do
  forM_ tests $ \(r, y, n) -> parse r $ \p -> do
    forM_ y $ \s -> it ("should match " ++ show s) $ testAnagrex p s
    forM_ n $ \s -> it ("should not match " ++ show s) $ not $ testAnagrex p s
  forM_ totals $ \(r, y) -> parse r $ \p -> do
    forM_ y $ \s -> it ("should match " ++ show s) $ testAnagrex p s
    it "should not match anything else" $ Q.property $ \s -> testAnagrex p s Q.=== (s `elem` y)
  forM_ props $ \(r, d, t) -> parse r $ it ("should " ++ d) . t
