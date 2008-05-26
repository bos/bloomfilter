module Data.BloomFilter.Util
    (
      nextPowerOfTwo
    , (:*)(..)
    ) where

import Data.Bits ((.|.), shiftR)

-- | An unboxed, strict pair type.
data a :* b = {-# UNPACK #-} !a :* {-# UNPACK #-} !b
            deriving (Eq, Ord, Show)

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
