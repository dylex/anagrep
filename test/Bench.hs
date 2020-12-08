import qualified Criterion.Main as C
import qualified Data.ByteString.Char8 as BSC

import Text.Regex.Anagram

dicts :: [String]
dicts = 
  [ "lt[aoeui]*"
  , "[lt]*[aoeui]{5,10}"
  , "[a-e][f-j][k-o][p-t][u-z]"
  , "[abc][def][ghi][jkl][mno][pqr][stu][vwx][yz]*"
  , "[a-e]?[f-j]?[k-o]?[p-t]?[u-z]?"
  , "[a-z]?[a-m]?[n-z]?[a-e]?[f-j]?[k-o]?[p-t]?[u-z]*"
  , "[a-m]{2,4}[a-g]{2,4}[a-d]{2,4}"
  , "[a-g][a-fz][a-ey-z][a-dx-z][a-cw-z][a-bv-z][au-z]"
  , "[a-g]?[a-fz]?[a-ey-z]?[a-dx-z]?[a-cw-z]?[a-bv-z]?[au-z]?"
  ]

main :: IO ()
main = C.defaultMain
  [ C.bgroup "parse" $ map (\d ->
      C.bench d $ C.nf makeAnagrex d)
      dicts
  , C.env (map BSC.unpack . BSC.lines <$> BSC.readFile "/usr/share/dict/words") $ \dict ->
    C.bgroup "dict" $ map (\d ->
      C.bench d $ C.nf (\(Right p) -> length $ filter (testAnagrex p) dict) (makeAnagrex d))
      dicts
  ]
