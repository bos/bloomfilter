{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Data.BloomFilter.Easy
-- Copyright: Bryan O'Sullivan
-- License: BSD3
--
-- Maintainer: Bryan O'Sullivan <bos@serpentine.com>
-- Stability: unstable
-- Portability: portable
--
-- An easy-to-use Bloom filter interface.

module Data.BloomFilter.Easy
    (
    -- * Easy creation and querying
      Bloom
    , easyList
    , elemB
    , notElemB
    , lengthB

    -- ** Example: a spell checker
    -- $example

    -- * Useful defaults for creation
    , safeSuggestSizing
    , suggestSizing
    ) where

import Data.BloomFilter (Bloom, elemB, fromListB, lengthB, notElemB)
import Data.BloomFilter.Hash (Hashable, cheapHashes)
import Data.BloomFilter.Util (nextPowerOfTwo)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB

-- | Create a Bloom filter with the given false positive rate and
-- members.  The hash functions used are computed by the @cheapHashes@
-- function from the 'Data.BloomFilter.Hash' module.
easyList :: (Hashable a)
         => Double              -- ^ desired false positive rate (0 < /e/ < 1)
         -> [a]                 -- ^ values to populate with
         -> Bloom a
{-# SPECIALIZE easyList :: Double -> [String] -> Bloom String #-}
{-# SPECIALIZE easyList :: Double -> [LB.ByteString] -> Bloom LB.ByteString #-}
{-# SPECIALIZE easyList :: Double -> [SB.ByteString] -> Bloom SB.ByteString #-}
easyList errRate xs = fromListB (cheapHashes numHashes) numBits xs
    where capacity = length xs
          (numBits, numHashes)
              | capacity > 0 = suggestSizing capacity errRate
              | otherwise    = (1, 1)

-- | Suggest a good combination of filter size and number of hash
-- functions for a Bloom filter, based on its expected maximum
-- capacity and a desired false positive rate.
--
-- The false positive rate is the rate at which queries against the
-- filter should return @True@ when an element is not actually
-- present.  It should be a fraction between 0 and 1, so a 1% false
-- positive rate is represented by 0.01.
safeSuggestSizing
    :: Int              -- ^ expected maximum capacity
    -> Double           -- ^ desired false positive rate (0 < /e/ < 1)
    -> Either String (Int, Int)
safeSuggestSizing capacity errRate
    | capacity <= 0                = Left "invalid capacity"
    | errRate <= 0 || errRate >= 1 = Left "invalid error rate"
    | otherwise =
    let cap = fromIntegral capacity
        (bits :: Double, hashes :: Double) =
            minimum [((-k) * cap / log (1 - (errRate ** (1 / k))), k)
                     | k <- [1..100]]
        roundedBits = nextPowerOfTwo (ceiling bits)
    in if roundedBits <= 0
       then Left  "capacity too large to represent"
       else Right (roundedBits, truncate hashes)

-- | Behaves as 'safeSuggestSizing', but calls 'error' if given
-- invalid or out-of-range inputs.
suggestSizing :: Int            -- ^ expected maximum capacity
              -> Double         -- ^ desired false positive rate (0 < /e/ < 1)
              -> (Int, Int)
suggestSizing cap errs = either fatal id (safeSuggestSizing cap errs)
  where fatal = error . ("Data.BloomFilter.Util.suggestSizing: " ++)

-- $example
--
-- This example reads a dictionary file containing one word per line,
-- constructs a Bloom filter with a 1% false positive rate, and
-- spellchecks its standard input.  Like the Unix @spell@ command, it
-- prints each word that it does not recognize.
--
-- @
--import Data.BloomFilter.Easy (easyList, elemB)
--
--main = do
--  filt <- ('easyList' 0.01 . words) \`fmap\` readFile \"/usr/share/dict/words\"
--  let check word | 'elemB' word filt = \"\"
--                 | otherwise         = word ++ \"\\n\"
--  interact (concat . map check . lines)
-- @
