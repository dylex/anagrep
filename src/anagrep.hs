{-# LANGUAGE RecordWildCards #-}

import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.CaseInsensitive (FoldCase, foldCase)
import qualified System.Console.GetOpt as Opt
import           System.Environment (getArgs, getProgName)
import           System.Exit (exitFailure)
import           System.IO (hPutStrLn, stderr)

import Text.Regex.Anagram

data Opts = Opts
  { optText, optCI :: Bool }

defOpts :: Opts
defOpts = Opts True False

options :: [Opt.OptDescr (Opts -> Opts)]
options =
  [ Opt.Option "b" []
      (Opt.NoArg (\o -> o{ optText = False }))
      "treat input as raw byte sequence (uses locale encoding by default)"
  , Opt.Option "i" []
      (Opt.NoArg (\o -> o{ optCI = True }))
      "ignore case distinctions in patterns and data"
  ]

main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs
  (Opts{..}, pat, files) <- case Opt.getOpt Opt.Permute options args of
    (o, r:f, []) -> case makeAnagrex r of
      Right p -> return (foldl (flip ($)) defOpts o, p, f)
      Left e -> do
        hPutStrLn stderr e
        exitFailure
    (_, _, e) -> do
      mapM_ (hPutStrLn stderr) e
      hPutStrLn stderr $ Opt.usageInfo ("Usage: " ++ prog ++ " REGEXP [FILE]...\n\
\Print lines in each FILE (or stdin) for which some permutation (anagram)\n\
\matches the given REGEXP.  REGEXP is a restricted regular expression that can\n\
\contain the following patterns:\n\
\  Character matches\n\
\    x         single literal character\n\
\    [aein-z]  character set (any listed character)\n\
\    [^a-mou]  negated character set (any character not listed)\n\
\    .         any single character\n\
\    \\x        escape single literal character (no special meanings)\n\
\  Repeat modifiers - may only be applied to characters (above)\n\
\    {N,M}     repeat character N-M times\n\
\    {N,}      repeat character at least N times\n\
\    {N}       equivalent to {N,N}\n\
\    ?         equivalent to {0,1}\n\
\    *         equivalent to {0,}\n\
\    +         equivalent to {1,}\n\
\  Combination\n\
\    XY        concatenation matches pattern X and Y in either order\n\
\    X|Y       alternation matches pattern X or Y\n\
\    (X)       grouping (only useful for alternation - note that successive\n\
\              grouped alternations involve a cross-product expansion and may\n\
\              be slow)\n\
\Other regular expression features are not currently supported.  Matching is\n\
\always done on entire lines (like grep -x).\n\
\\n\
\Example: " ++ prog ++ " 'pq[aeiou]{4,}' /usr/share/dict/words\n\
\  > opaque\n\
\\n\
\Flags:") options
      exitFailure
  let ci :: FoldCase a => a -> a
      ci
        | optCI = foldCase
        | otherwise = id
      grex = ci pat
      test = testAnagrex grex . ci
      proc f
        | optText = mapM_ putStrLn . filter test . lines =<< maybe getContents readFile f
        | otherwise = mapM_ BSLC.putStrLn . filter (test . BSLC.unpack) . BSLC.lines =<< maybe BSLC.getContents BSLC.readFile f
  case files of
    [] -> proc Nothing
    _ -> mapM_ (proc . Just) files
