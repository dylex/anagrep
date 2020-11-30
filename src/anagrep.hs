{-# LANGUAGE TupleSections #-}

import           Control.Monad ((<=<))
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           System.Environment (getArgs, getProgName)
import           System.Exit (exitFailure)
import           System.IO (hPutStrLn, stderr)

import Text.Regex.Anagram

-- TODO: string (unicode), caseins

main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs
  (pat, fl) <- either (\e -> hPutStrLn stderr e >> exitFailure) return $ case args of
    (r:f) -> (, f) <$> parseAnagrex r
    _ -> Left ("Usage: " ++ prog ++ " SEQREGEXP [FILE]...")
  let proc = mapM_ BSLC.putStrLn . filter (testAnagrex pat . BSLC.unpack) . BSLC.lines
  case fl of
    [] -> proc =<< BSLC.getContents
    _ -> mapM_ (proc <=< BSLC.readFile) fl
