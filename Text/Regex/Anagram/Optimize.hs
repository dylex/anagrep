{-# LANGUAGE RecordWildCards #-}

module Text.Regex.Anagram.Optimize
  ( optimizeAnagrex
  ) where

import           Control.Monad (mfilter)
import           Data.Maybe (mapMaybe)

import Text.Regex.Anagram.Types
import Text.Regex.Anagram.Util

-- |Remove everything in patStars from patOpts
filterStar :: Pat -> Pat
filterStar p@Pat{..} = p
  { patOpts = mapMaybe (mfilter (not . nullChar) . Just . intersectChar (notChar patStars)) patOpts
  }

optimizePat :: Pat -> Pat
optimizePat = filterStar

optimizeAlts :: [Pat] -> [Pat]
optimizeAlts = map optimizePat

optimizeAnagrex :: Anagrex -> Anagrex
optimizeAnagrex (Anagrex p) = Anagrex $ optimizeAlts p
