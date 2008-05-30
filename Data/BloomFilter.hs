{-# LANGUAGE Rank2Types, TypeOperators #-}

-- |
-- Module: Data.BloomFilter
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
-- * False negatives /are/ possible.  If an element has not been added
--   to a filter, a membership test /may/ nevertheless indicate that
--   the element is present.
--
-- This module provides low-level control.  For an easier to use
-- interface, see the "Data.BloomFilter.Easy" module.

module Data.BloomFilter
    (
    -- * Overview
    -- $overview

    -- ** Ease of use
    -- $ease

    -- ** Performance
    -- $performance

    -- * Types
      Hash
    , Bloom
    , MBloom

    -- * Immutable Bloom filters
    -- ** Creation
    , unfoldB
    , fromListB
    , createB

    -- ** Accessors
    , lengthB
    , elemB

    -- * Mutable Bloom filters
    -- ** Creation
    , newMB
    , unsafeFreezeMB
    , thawMB

    -- ** Accessors
    , lengthMB
    , elemMB

    -- ** Mutation
    , insertMB

    -- * The underlying representation
    -- | If you serialize the raw bit arrays below to disk, do not
    -- expect them to be portable to systems with different
    -- conventions for endianness or word size.

    -- | The raw bit array used by the immutable 'Bloom' type.
    , bitArrayB

    -- | The raw bit array used by the immutable 'MBloom' type.
    , bitArrayMB
    ) where

import Control.Monad (liftM, forM_)
import Control.Monad.ST (ST, runST)
import Data.Array.Base (unsafeAt, unsafeRead, unsafeWrite)
import Data.Array.ST (STUArray, newArray, thaw, unsafeFreeze)
import Data.Array.Unboxed (UArray)
import Data.Bits ((.&.), (.|.))
import Data.BloomFilter.Util (FastShift(..), (:*)(..), nextPowerOfTwo)
import Data.Word (Word32)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB

-- Make sure we're not performing any expensive arithmetic operations.
import Prelude hiding ((/), (*), div, divMod, mod, rem)

{-
import Debug.Trace
traceM :: (Show a, Monad m) => a -> m ()
traceM v = show v `trace` return ()
traces :: Show a => a -> b -> b
traces s = trace (show s)
-}

-- | A hash value is 32 bits wide.  This limits the maximum size of a
-- filter to about four billion elements, or 512 megabytes of memory.
type Hash = Word32

-- | A mutable Bloom filter, for use within the 'ST' monad.
data MBloom s a = MB {
      hashMB :: {-# UNPACK #-} !(a -> [Hash])
    , shiftMB :: {-# UNPACK #-} !Int
    , maskMB :: {-# UNPACK #-} !Int
    , bitArrayMB :: {-# UNPACK #-} !(STUArray s Int Hash)
    }

-- | An immutable Bloom filter, suitable for querying from pure code.
data Bloom a = B {
      hashB :: {-# UNPACK #-} !(a -> [Hash])
    , shiftB :: {-# UNPACK #-} !Int
    , maskB :: {-# UNPACK #-} !Int
    , bitArrayB :: {-# UNPACK #-} !(UArray Int Hash)
    }

instance Show (MBloom s a) where
    show mb = "MBloom { " ++ show (lengthMB mb) ++ " bits } "

instance Show (Bloom a) where
    show ub = "Bloom { " ++ show (lengthB ub) ++ " bits } "

-- | Create a new mutable Bloom filter.  For efficiency, the number of
-- bits used may be larger than the number requested.  It is always
-- rounded up to the nearest higher power of two.
--
-- For a safer creation interface, use 'createB'.  To convert a
-- mutable filter to an immutable filter for use in pure code, use
-- 'unsafeFreezeMB'.
newMB :: (a -> [Hash])          -- ^ family of hash functions to use
      -> Int                    -- ^ number of bits in filter
      -> ST s (MBloom s a)
newMB hash numBits = MB hash shift mask `liftM` newArray (0, numElems - 1) 0
  where twoBits | numBits < 1 = 1
                | isPowerOfTwo numBits = numBits
                | otherwise = nextPowerOfTwo numBits
        numElems = max 2 (twoBits `shiftR` logBitsInHash)
        trueBits = numElems `shiftL` logBitsInHash
        shift = logPower2 trueBits
        mask = trueBits - 1
        isPowerOfTwo n = n .&. (n - 1) == 0

logBitsInHash :: Int
logBitsInHash = 5 -- logPower2 bitsInHash

-- | Create an immutable Bloom filter, using the given setup function
-- which executes in the 'ST' monad.
--
-- Example:
--
-- @
--import "Data.BloomFilter.Hash" (cheapHashes)
--
--filter = createB (cheapHashes 3) 1024 $ \mf -> do
--           insertMB mf \"foo\"
--           insertMB mf \"bar\"
-- @
--
-- Note that the result of the setup function is not used.
createB :: (a -> [Hash])        -- ^ family of hash functions to use
        -> Int                  -- ^ number of bits in filter
        -> (forall s. (MBloom s a -> ST s z))  -- ^ setup function (result is discarded)
        -> Bloom a
{-# INLINE createB #-}
createB hash numBits body = runST $ do
  mb <- newMB hash numBits
  body mb
  unsafeFreezeMB mb

-- | Given a filter's mask and a hash value, compute an offset into
-- a word array and a bit offset within that word.
hashIdx :: Int -> Word32 -> (Int :* Int)
hashIdx mask x = (y `shiftR` logBitsInHash) :* (y .&. hashMask)
  where hashMask = 31 -- bitsInHash - 1
        y = fromIntegral x .&. mask

-- | Hash the given value, returning a list of (word offset, bit
-- offset) pairs, one per hash value.
hashesM :: MBloom s a -> a -> [Int :* Int]
hashesM mb elt = hashIdx (maskMB mb) `map` hashMB mb elt

-- | Hash the given value, returning a list of (word offset, bit
-- offset) pairs, one per hash value.
hashesU :: Bloom a -> a -> [Int :* Int]
hashesU ub elt = hashIdx (maskB ub) `map` hashB ub elt

-- | Insert a value into a mutable Bloom filter.  Afterwards, a
-- membership query for the same value is guaranteed to return @True@.
insertMB :: MBloom s a -> a -> ST s ()
{-# SPECIALIZE insertMB :: MBloom s SB.ByteString -> SB.ByteString -> ST s () #-}
{-# SPECIALIZE insertMB :: MBloom s LB.ByteString -> LB.ByteString -> ST s () #-}
{-# SPECIALIZE insertMB :: MBloom s String -> String -> ST s () #-}
insertMB mb elt = do
  let mu = bitArrayMB mb
  forM_ (hashesM mb elt) $ \(word :* bit) -> do
      old <- unsafeRead mu word
      unsafeWrite mu word (old .|. (1 `shiftL` bit))

-- | Query a mutable Bloom filter for membership.  If the value is
-- present, return @True@.  If the value is not present, there is
-- /still/ some possibility that @True@ will be returned.
elemMB :: a -> MBloom s a -> ST s Bool
elemMB elt mb = loop (hashesM mb elt)
  where mu = bitArrayMB mb
        loop ((word :* bit):wbs) = do
          i <- unsafeRead mu word
          if i .&. (1 `shiftL` bit) == 0
            then return False
            else loop wbs
        loop _ = return True

-- | Query an immutable Bloom filter for membership.  If the value is
-- present, return @True@.  If the value is not present, there is
-- /still/ some possibility that @True@ will be returned.
elemB :: a -> Bloom a -> Bool
elemB elt ub = all test (hashesU ub elt)
  where test (off :* bit) = (bitArrayB ub `unsafeAt` off) .&. (1 `shiftL` bit) /= 0
          
-- | Create an immutable Bloom filter from a mutable one.  The mutable
-- filter /must not/ be modified afterwards, or a runtime crash may
-- occur.  For a safer creation interface, use 'createB'.
unsafeFreezeMB :: MBloom s a -> ST s (Bloom a)
unsafeFreezeMB mb = B (hashMB mb) (shiftMB mb) (maskMB mb) `liftM`
                    unsafeFreeze (bitArrayMB mb)

-- | Copy an immutable Bloom filter to create a mutable one.  There is
-- no non-copying equivalent.
thawMB :: Bloom a -> ST s (MBloom s a)
thawMB ub = MB (hashB ub) (shiftB ub) (maskB ub) `liftM` thaw (bitArrayB ub)

-- bitsInHash :: Int
-- bitsInHash = sizeOf (undefined :: Hash) `shiftL` 3

-- | Return the size of a mutable Bloom filter, in bits.
lengthMB :: MBloom s a -> Int
lengthMB = shiftL 1 . shiftMB

-- | Return the size of an immutable Bloom filter, in bits.
lengthB :: Bloom a -> Int
lengthB = shiftL 1 . shiftB

-- | Build an immutable Bloom filter from a seed value.  The seeding
-- function populates the filter as follows.
--
--   * If it returns 'Nothing', it is finished producing values to
--     insert into the filter.
--
--   * If it returns @'Just' (a,b)@, @a@ is added to the filter and
--     @b@ is used as a new seed.
unfoldB :: (a -> [Hash])        -- ^ family of hash functions to use
        -> Int                  -- ^ number of bits in filter
        -> (b -> Maybe (a, b))  -- ^ seeding function
        -> b                    -- ^ initial seed
        -> Bloom a
{-# INLINE unfoldB #-}
unfoldB hashes numBits f k = createB hashes numBits (loop k)
  where loop j mb = case f j of
                      Just (a, j') -> insertMB mb a >> loop j' mb
                      _ -> return ()

-- | Create an immutable Bloom filter, populating it from a list of
-- values.
--
-- Here is an example that uses the @cheapHashes@ function from the
-- "Data.BloomFilter.Hash" module to create a hash function that
-- returns three hashes.
--
-- @
--import "Data.BloomFilter.Hash" (cheapHashes)
--
--filt = fromListB (cheapHashes 3) 1024 [\"foo\", \"bar\", \"quux\"]
-- @
fromListB :: (a -> [Hash])      -- ^ family of hash functions to use
          -> Int                -- ^ number of bits in filter
          -> [a]                -- ^ values to populate with
          -> Bloom a
{-# INLINE fromListB #-}
fromListB hashes numBits list = createB hashes numBits (loop list)
  where loop (x:xs) mb = insertMB mb x >> loop xs mb
        loop _ _       = return ()

{-
-- This is a simpler definition, but GHC doesn't inline the unfold
-- sensibly.

fromListB hashes numBits = unfoldB hashes numBits convert
  where convert (x:xs) = Just (x, xs)
        convert _      = Nothing
-}

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
