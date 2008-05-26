{-# OPTIONS_GHC -fglasgow-exts #-}

module Data.BloomFilter.Util
    (
      FastShift(..)
    , nextPowerOfTwo
    , (:*)(..)
    ) where

import Data.Bits ((.|.))
import qualified Data.Bits as Bits
import Foreign.C.Types (CSize)
import GHC.Base
import GHC.Word

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

class FastShift a where
    shiftL :: a -> Int -> a
    shiftR :: a -> Int -> a

instance FastShift Word32 where
    {-# INLINE shiftL #-}
    shiftL (W32# x#) (I# i#) = W32# (x# `uncheckedShiftL#` i#)

    {-# INLINE shiftR #-}
    shiftR (W32# x#) (I# i#) = W32# (x# `uncheckedShiftRL#` i#)

instance FastShift Int where
    {-# INLINE shiftL #-}
    shiftL (I# x#) (I# i#) = I# (x# `iShiftL#` i#)

    {-# INLINE shiftR #-}
    shiftR (I# x#) (I# i#) = I# (x# `iShiftRA#` i#)

instance FastShift CSize where
    shiftL = Bits.shiftL
    shiftR = Bits.shiftR

instance FastShift Integer where
    shiftL = Bits.shiftL
    shiftR = Bits.shiftR
