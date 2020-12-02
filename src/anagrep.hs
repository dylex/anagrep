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
    (o, r:f, []) -> case parseAnaPattern r of
      Right p -> return (foldl (flip ($)) defOpts o, p, f)
      Left e -> do
        hPutStrLn stderr e
        exitFailure
    (_, _, e) -> do
      mapM_ (hPutStrLn stderr) e
      hPutStrLn stderr $ Opt.usageInfo ("Usage: " ++ prog ++ " REGEXP [FILE]...\nSearch for anagrams of REGEXP in each FILE (or stdin).") options
      exitFailure
  let ci :: FoldCase a => a -> a
      ci
        | optCI = foldCase
        | otherwise = id
      grex = compileAnagrex (ci pat)
      test = testAnagrex grex . ci
      proc f
        | optText = mapM_ putStrLn . filter test . lines =<< maybe getContents readFile f
        | otherwise = mapM_ BSLC.putStrLn . filter (test . BSLC.unpack) . BSLC.lines =<< maybe BSLC.getContents BSLC.readFile f
  case files of
    [] -> proc Nothing
    _ -> mapM_ (proc . Just) files
