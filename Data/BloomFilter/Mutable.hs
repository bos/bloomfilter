{-# LANGUAGE BangPatterns, CPP, Rank2Types,
    TypeOperators #-}

-- |
-- Module: Data.BloomFilter.Mutable
-- Copyright: Bryan O'Sullivan
-- License: BSD3
--
-- Maintainer: Bryan O'Sullivan <bos@serpentine.com>
-- Stability: unstable
-- Portability: portable
--
-- A fast, space efficient Bloom filter implementation.  A Bloom
-- filter is a set-like data structure that provides a probabilistic
-- membership test.
--
-- * Queries do not give false negatives.  When an element is added to
--   a filter, a subsequent membership test will definitely return
--   'True'.
--
-- * False positives /are/ possible.  If an element has not been added
--   to a filter, a membership test /may/ nevertheless indicate that
--   the element is present.
--
-- This module provides low-level control.  For an easier to use
-- interface, see the "Data.BloomFilter.Easy" module.

module Data.BloomFilter.Mutable
    (
    -- * Overview
    -- $overview

    -- ** Ease of use
    -- $ease

    -- ** Performance
    -- $performance

    -- * Types
      Hash
    , MBloom
    -- * Mutable Bloom filters

    -- ** Creation
    , new

    -- ** Accessors
    , length
    , elem

    -- ** Mutation
    , insert

    -- * The underlying representation
    -- | If you serialize the raw bit arrays below to disk, do not
    -- expect them to be portable to systems with different
    -- conventions for endianness or word size.

    -- | The raw bit array used by the immutable 'MBloom' type.
    , bitArray
    ) where

#include "MachDeps.h"

import Control.Monad (liftM, forM_)
import Control.Monad.ST (ST)
import Data.Array.Base (unsafeRead, unsafeWrite)
import Data.Array.ST (thaw, unsafeFreeze)
import Data.Bits ((.&.), (.|.))
import Data.BloomFilter.Array (newArray)
import Data.BloomFilter.Util (FastShift(..), (:*)(..), nextPowerOfTwo)
import Data.Word (Word32)
import Data.BloomFilter.Mutable.Internal

import Prelude hiding (elem, length, notElem,
                       (/), (*), div, divMod, mod, rem)

-- | Create a new mutable Bloom filter.  For efficiency, the number of
-- bits used may be larger than the number requested.  It is always
-- rounded up to the nearest higher power of two, but clamped at a
-- maximum of 4 gigabits, since hashes are 32 bits in size.
--
-- For a safer creation interface, use 'create'.  To convert a
-- mutable filter to an immutable filter for use in pure code, use
-- 'unsafeFreeze'.
new :: (a -> [Hash])          -- ^ family of hash functions to use
      -> Int                    -- ^ number of bits in filter
      -> ST s (MBloom s a)
new hash numBits = MB hash shift mask `liftM` newArray numElems numBytes
  where twoBits | numBits < 1 = 1
                | numBits > maxHash = maxHash
                | isPowerOfTwo numBits = numBits
                | otherwise = nextPowerOfTwo numBits
        numElems = max 2 (twoBits `shiftR` logBitsInHash)
        numBytes = numElems `shiftL` logBytesInHash
        trueBits = numElems `shiftL` logBitsInHash
        shift = logPower2 trueBits
        mask = trueBits - 1
        isPowerOfTwo n = n .&. (n - 1) == 0

maxHash :: Int
#if WORD_SIZE_IN_BITS == 64
maxHash = 4294967296
#else
maxHash = 1073741824
#endif

logBitsInHash :: Int
logBitsInHash = 5 -- logPower2 bitsInHash

logBytesInHash :: Int
logBytesInHash = 2 -- logPower2 (sizeOf (undefined :: Hash))

-- | Given a filter's mask and a hash value, compute an offset into
-- a word array and a bit offset within that word.
hashIdx :: Int -> Word32 -> (Int :* Int)
hashIdx mask x = (y `shiftR` logBitsInHash) :* (y .&. hashMask)
  where hashMask = 31 -- bitsInHash - 1
        y = fromIntegral x .&. mask

-- | Hash the given value, returning a list of (word offset, bit
-- offset) pairs, one per hash value.
hashesM :: MBloom s a -> a -> [Int :* Int]
hashesM mb elt = hashIdx (mask mb) `map` hashes mb elt

-- | Insert a value into a mutable Bloom filter.  Afterwards, a
-- membership query for the same value is guaranteed to return @True@.
insert :: MBloom s a -> a -> ST s ()
insert mb elt = do
  let mu = bitArray mb
  forM_ (hashesM mb elt) $ \(word :* bit) -> do
      old <- unsafeRead mu word
      unsafeWrite mu word (old .|. (1 `shiftL` bit))

-- | Query a mutable Bloom filter for membership.  If the value is
-- present, return @True@.  If the value is not present, there is
-- /still/ some possibility that @True@ will be returned.
elem :: a -> MBloom s a -> ST s Bool
elem elt mb = loop (hashesM mb elt)
  where mu = bitArray mb
        loop ((word :* bit):wbs) = do
          i <- unsafeRead mu word
          if i .&. (1 `shiftL` bit) == 0
            then return False
            else loop wbs
        loop _ = return True

-- bitsInHash :: Int
-- bitsInHash = sizeOf (undefined :: Hash) `shiftL` 3

-- | Return the size of a mutable Bloom filter, in bits.
length :: MBloom s a -> Int
length = shiftL 1 . shift


-- | Slow, crummy way of computing the integer log of an integer known
-- to be a power of two.
logPower2 :: Int -> Int
logPower2 k = go 0 k
    where go j 1 = j
          go j n = go (j+1) (n `shiftR` 1)

-- $overview
--
-- Each of the functions for creating Bloom filters accepts two parameters:
--
-- * The number of bits that should be used for the filter.  Note that
--   a filter is fixed in size; it cannot be resized after creation.
--
-- * A function that accepts a value, and should return a fixed-size
--   list of hashes of that value.  To keep the false positive rate
--   low, the hashes computes should, as far as possible, be
--   independent.
--
-- By choosing these parameters with care, it is possible to tune for
-- a particular false positive rate.  The @suggestSizing@ function in
-- the "Data.BloomFilter.Easy" module calculates useful estimates for
-- these parameters.

-- $ease
--
-- This module provides both mutable and immutable interfaces for
-- creating and querying a Bloom filter.  It is most useful as a
-- low-level way to create a Bloom filter with a custom set of
-- characteristics, perhaps in combination with the hashing functions
-- in 'Data.BloomFilter.Hash'.
--
-- For a higher-level interface that is easy to use, see the
-- 'Data.BloomFilter.Easy' module.

-- $performance
--
-- The implementation has been carefully tuned for high performance
-- and low space consumption.
--
-- For efficiency, the number of bits requested when creating a Bloom
-- filter is rounded up to the nearest power of two.  This lets the
-- implementation use bitwise operations internally, instead of much
-- more expensive multiplication, division, and modulus operations.
