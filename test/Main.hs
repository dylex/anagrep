import           Control.Monad (forM_)
import qualified Data.Bits as B
import qualified Data.CaseInsensitive as CI
import           Data.Either (isRight)
import           Numeric.Natural (Natural)
import           Test.Hspec
import qualified Test.QuickCheck as Q

import Text.Regex.Anagram
import qualified Text.Regex.Anagram.Bits as B

tests :: [(Bool, String, [String], [String])]
tests =
  [ (False, "a*", ["","a","aa","aaa","aaaaaaaaaaaaaa"],
      ["b","ab","bad","A"])
  , (False, "a+b", ["ba","ab","baaaa","aaba"],
      ["","bab","abc","b"])
  , (False, "a[ab]+b", ["baa","aab","aba","abb","bba","bab","aaaba","bbba"],
      ["","ab","aa","aaa","b","xabb"])
  , (False, "a[ab]*b?", ["baa","aab","aba","abb","bba","bab","aaaba","bbba","aa","ab","aaa","a"],
      ["","bbb","b","abc"])
  , (False, "ba{2,}", ["aba","aaab","baaaa","aaaaabaaa"],
      ["a","ab","baac"])
  , (False, "[^a]", ["b","c","d"],
      ["","a","bb","ba"])
  , (False, "[^a-d]*", ["ee","xyz",""],
      ["a","xa","fce"])
  , (False, "[^a][^ab][^abc]", ["bcd","cde","xyz","ccd"],
      ["","a","ab","abc","aaa","bbb","ccc"])
  , (False, "[a-c][^a-c]", ["ad","xc"],
      ["","a","cc","aa","xy","ab"])
  , (False, "[abc]?[ab]?a?", ["","a","b","c","aa","ab","ba","ac","ca","aaa","abc","cab"],
      ["bbc","abcabcabcabc","aaaaa","abcdefg","x"])
  , (False, "[efg][def][cde][bcd][abc]", ["abcde","ccffc","gfedc","defbc"],
      ["","ddd","aaaaa","ccccc","cccae"])
  , (False, "[efg][^def][cde][^bcd][abc]", ["abcde","ccffc","gfedc","defbc","cccae"],
      ["","ddd","aaaaa","ccccc"])
  , (False, "[a-m]{10}", ["abcdefghij","cdfmeadljb","eeeeeeeeee","aaaabbbaaa"],
      ["a","mnopqrstuv","abcdefghix","aaaaaaaaan","abcdefghijklm"])
  , (False, "[a-m]{10}a?", ["abcdefghij","cdfmeadljb","aeeeeeeeeee","aaaabbbaaa","abcdefghijk"],
      ["a","mnopqrstuv","abcdefghix","aaaaaaaaan","abcdefghijklm","abcdefghij1"])
  , (False, "[a-m]{1,10}", ["a","b","m","cde","abcdefghij","cdfmeadljb","eeeeeeeeee","aaaabbb"],
      ["","xyz","abcdefghix","aaaaaxaaaa","abcdefghijklm","aabbccddeeffgghhiijj","abcx"])
  , (False, "[a-m]{13,26}", ["abcdefghijklm","cdfmeadljbjmeab","eeeeeeeeeeeeeeeeeeee","aaaabbbaaaccccaaddddaa","abcdefghijklmmlkjihgfedcba"],
      ["a","mnopqrstuvwxyz","abcdefghijklmabcdefgh1","aaaaaaaaaaaaaaaan","abcdefghijklmmmmmmmmmmn"])
  , (False, "[a-m][a-ln][a-kn-o][a-jn-p][a-in-q][a-hn-r][a-gn-s][a-fn-t][a-en-u][a-dn-v][a-cn-w][a-bn-x][an-y][n-z]", ["abcdefghijklmn", "bcdefghijklmno", "mnopqrstuvwxyz"],
      -- pathologically slow
      ["aaaaaaaaaaaaaa","abcdefghijklm","nnnnnnnnnnnnn","abcdefghijklma"])
  , (False, "[0-9]?[0-8a]?[0-7a-b]?[0-6a-c]?[0-5a-d]?[0-4a-e]?[0-3a-f]?[0-2a-g]?[0-1a-h]?[0a-i]?", ["0123456789", "123456789i", "0000000000", "abcdefghi9", "000000000", "abcdefghi", "", "i", "fab9"],
      ["1111111111","1234567899"])
  , (True, "a*", ["","a","A","aAa"],
      ["b"])
  , (True, "[^A]", ["b"],
      ["","a","A"])
  , (True, "[A-C][^a-c]", ["ad","xc","Ad","XC"],
      ["","a","cc","aa","xy","aA","AA"])
  ]

totals :: [(String, [String])]
totals =
  [ ("()", [""])
  , ("abc", ["abc","acb","bac","bca","cab","cba"])
  , ("[a-d]", ["a","b","c","d"])
  , ("[ab][bc]", ["ab","ac","bb","bc","ca","cb"])
  , ("[ab][ac]", ["aa","ac","ba","bc","ca","cb"])
  , ("a[ab]b", ["baa","aab","aba","abb","bba","bab"])
  , ("a[ab]?b?", ["a","ab","aa","baa","aab","aba","abb","bba","bab"])
  , ("[ab]a?", ["a","b","aa","ab","ba"])
  , ("[ab]{2}a?", ["aa","ab","ba","bb","aaa","aab","aba","baa","abb","bab","bba"])
  , ("(a|b)c", ["ac","ca","bc","cb"])
  , ("(ab|cd)", ["ab","ba","cd","dc"])
  , ("(a|b(b?|c))", ["a","b","bb","bc","cb"])
  , ("a{2,4}", ["aa","aaa","aaaa"])
  ]

props :: [(String, String, CI.CI Anagrex -> Q.Property)]
props =
  [ (".*", "match everything", \p -> Q.property $
      test False p)
  , (".+", "match anything", \p -> Q.property $
      \s -> test False p s Q.=/= null s)
  , (".*x", "match anything with an x", \p -> Q.property $
      \s -> test False p s Q.=== ('x' `elem` s))
  , ("a.*x", "match anything with an a and x", \p -> Q.property $
      \s -> test False p s Q.=== ('x' `elem` s && 'a' `elem` s))
  , (".", "match single chars", \p -> Q.property $
      \s -> test False p s Q.=== (length s == 1))
  , (".{2}", "match two chars", \p -> Q.property $
      \s -> test False p s Q.=== (length s == 2))
  ]

parse :: String -> (CI.CI Anagrex -> Spec) -> Spec
parse r t = describe ("pattern " ++ show r) $ do
  let pp = makeAnagrex r
      Right p = pp
  it "should parse" $ pp `shouldSatisfy` isRight
  t $ CI.mk p

test :: Bool -> CI.CI Anagrex -> String -> Bool
test False p s = testAnagrex (CI.original p) s
test True p s = testAnagrexCI p (CI.mk s)

main :: IO ()
main = hspec $ describe "Text.Regex.Anagram" $ do
  describe "findBits" $ do
    it "should find one bit" $ Q.forAll (Q.choose (0, 255)) $ \i -> B.findBits (B.bit i     :: Natural) Q.=== [i]
    it "should find all bit" $ Q.forAll (Q.choose (0, 255)) $ \i -> B.findBits (B.allBits i :: Natural) Q.=== [0..pred i]
  forM_ tests $ \(ci, r, y, n) -> parse r $ \p -> do
    forM_ y $ \s -> it ("should match " ++ show s) $ test ci p s
    forM_ n $ \s -> it ("should not match " ++ show s) $ not $ test ci p s
  forM_ totals $ \(r, y) -> parse r $ \p -> do
    forM_ y $ \s -> it ("should match " ++ show s) $ test False p s
    it "should not match anything else" $ Q.property $ \s -> test False p s Q.=== (s `elem` y)
  forM_ props $ \(r, d, t) -> parse r $ it ("should " ++ d) . t
