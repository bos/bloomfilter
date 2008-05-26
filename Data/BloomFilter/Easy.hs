{-# LANGUAGE PatternSignatures #-}

module Data.BloomFilter.Easy
    (
    -- * Easy construction and querying
      easyList
    , elemB
    , lengthB
    , suggestSizing
    ) where

import Data.BloomFilter (Bloom, elemB, fromListB, lengthB)
import Data.BloomFilter.Hash (Hashable, cheapHashes)
import Data.BloomFilter.Util (nextPowerOfTwo)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB

-- | Create a Bloom filter with the given false positive rate and
-- members.
easyList :: (Hashable a)
         => Double              -- ^ desired false positive rate (0 < /e/ < 1)
         -> [a]                 -- ^ values to populate with
         -> Bloom a
{-# SPECIALIZE easyList :: Double -> [String] -> Bloom String #-}
{-# SPECIALIZE easyList :: Double -> [LB.ByteString] -> Bloom LB.ByteString #-}
{-# SPECIALIZE easyList :: Double -> [SB.ByteString] -> Bloom SB.ByteString #-}
easyList errRate xs =
    let capacity = length xs
        (numBits, numHashes) = suggestSizing capacity errRate
    in fromListB (cheapHashes numHashes) numBits xs

-- | Suggest the best combination of filter size and number of hash
-- functions for a Bloom filter, based on its expected maximum
-- capacity and a desired false positive rate.
--
-- The false positive rate is the rate at which queries against the
-- filter should return @True@ when an element is not actually
-- present.
suggestSizing :: Int            -- ^ expected maximum capacity
              -> Double         -- ^ desired false positive rate (0 < /e/ < 1)
              -> (Int, Int)
suggestSizing capacity errRate
    | capacity <= 0 = fatal "invalid capacity"
    | errRate <= 0 || errRate >= 1 = fatal "invalid error rate"
    | otherwise =
    let cap = fromIntegral capacity
        (bits :: Double, hashes :: Double) =
            minimum [((-k) * cap / log (1 - (errRate ** (1 / k))), k)
                     | k <- [1..100]]
    in (nextPowerOfTwo (round bits), round hashes)
  where fatal = error . ("Data.BloomFilter.Util.suggestSizing: " ++)
