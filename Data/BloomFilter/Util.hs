{-# LANGUAGE BangPatterns, MagicHash, TypeOperators #-}

module Data.BloomFilter.Util
    (
      nextPowerOfTwo
    , (:*)(..)
    ) where

import Data.Bits ((.|.), unsafeShiftR)

-- | A strict pair type.
data a :* b = !a :* !b
            deriving (Eq, Ord, Show)

-- | Compute the nearest power of two greater to or equal than the
-- given number.
nextPowerOfTwo :: Int -> Int
{-# INLINE nextPowerOfTwo #-}
nextPowerOfTwo n =
    let a = n - 1
        b = a .|. (a `unsafeShiftR` 1)
        c = b .|. (b `unsafeShiftR` 2)
        d = c .|. (c `unsafeShiftR` 4)
        e = d .|. (d `unsafeShiftR` 8)
        f = e .|. (e `unsafeShiftR` 16)
        g = f .|. (f `unsafeShiftR` 32)  -- in case we're on a 64-bit host
        !h = g + 1
    in h
