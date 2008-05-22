module Data.BloomFilter.Util
    (
      nextPowerOfTwo
    , suggestSizing
    ) where

import Data.Bits ((.|.), shiftR)

-- | Suggest the best combination of filter size and number of hash
-- functions for a Bloom filter, based on its expected maximum
-- capacity and a desired error rate.
suggestSizing :: Int            -- ^ expected maximum capacity
              -> Double         -- ^ desired error rate (0 < e < 1)
              -> (Int, Int)
suggestSizing capacity errRate
    | capacity <= 0 = fatal "invalid capacity"
    | errRate <= 0 || errRate >= 1 = fatal "invalid error rate"
    | otherwise =
    let cap = fromIntegral capacity
        bits, hashes :: Double
        (bits, hashes) =
            minimum [((-k) * cap / log (1 - (errRate ** (1 / k))), k)
                     | k <- [1..100]]
    in (nextPowerOfTwo (round bits), round hashes)
  where fatal = error . ("Data.BloomFilter.Util.suggestSizing: " ++)

-- | Compute the nearest power of two greater to or equal than the
-- given number.
nextPowerOfTwo :: Int -> Int
{-# INLINE nextPowerOfTwo #-}
nextPowerOfTwo n =
    let a = n - 1
        b = a .|. (a `shiftR` 1)
        c = b .|. (b `shiftR` 2)
        d = c .|. (c `shiftR` 4)
        e = d .|. (d `shiftR` 8)
        f = e .|. (e `shiftR` 16)
        g = f .|. (f `shiftR` 32)  -- in case we're on a 64-bit host
        !h = g + 1
    in h
