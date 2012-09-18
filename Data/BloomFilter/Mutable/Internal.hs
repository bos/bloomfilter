-- |
-- Module: Data.BloomFilter.Mutable.Internal
-- Copyright: Bryan O'Sullivan
-- License: BSD3
--
-- Maintainer: Bryan O'Sullivan <bos@serpentine.com>
-- Stability: unstable
-- Portability: portable

module Data.BloomFilter.Mutable.Internal
    (
    -- * Types
      Hash
    , MBloom(..)
    ) where

import Data.Array.Base (STUArray)
import Data.Bits (shiftL)
import Data.Word (Word32)

import Prelude hiding (elem, length, notElem,
                       (/), (*), div, divMod, mod, rem)

-- | A hash value is 32 bits wide.  This limits the maximum size of a
-- filter to about four billion elements, or 512 megabytes of memory.
type Hash = Word32

-- | A mutable Bloom filter, for use within the 'ST' monad.
data MBloom s a = MB {
      hashes :: !(a -> [Hash])
    , shift :: {-# UNPACK #-} !Int
    , mask :: {-# UNPACK #-} !Int
    , bitArray :: {-# UNPACK #-} !(STUArray s Int Hash)
    }

instance Show (MBloom s a) where
    show mb = "MBloom { " ++ show ((1::Int) `shiftL` shift mb) ++ " bits } "
