{-# LANGUAGE Rank2Types, TypeOperators #-}

module Data.BloomFilter
    (
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
    , bitArrayB
    , bitArrayMB
    ) where

import Control.Monad (liftM, forM_)
import Control.Monad.ST (ST, runST)
import Data.Array.Base (unsafeAt, unsafeRead, unsafeWrite)
import Data.Array.ST (STUArray, newArray, thaw, unsafeFreeze)
import Data.Array.Unboxed (UArray)
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.BloomFilter.Util
import Data.Word (Word32)

-- Make sure we're not performing any expensive arithmetic operations.
import Prelude hiding ((/), (*), div, divMod, mod, rem)

{-
import Debug.Trace
traceM :: (Show a, Monad m) => a -> m ()
traceM v = show v `trace` return ()
traces :: Show a => a -> b -> b
traces s = trace (show s)
-}

type Hash = Word32

-- | Mutable Bloom filter, for use within the 'ST' monad.
data MBloom s a = MB {
      hashMB :: {-# UNPACK #-} !(a -> [Hash])
    , shiftMB :: {-# UNPACK #-} !Int
    , maskMB :: {-# UNPACK #-} !Int
    , bitArrayMB :: {-# UNPACK #-} !(STUArray s Int Hash)
    }

-- | Immutable Bloom filter, suitable for querying from pure code.
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

-- | Create an immutable Bloom filter, using the given setup function.
createB :: (a -> [Hash])        -- ^ family of hash functions to use
        -> Int                  -- ^ number of bits in filter
        -> (forall s. (MBloom s a -> ST s ()))  -- ^ setup function
        -> Bloom a
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
--   * If it returns 'Nothing', it is done producing values to insert
--     into the filter.
--   * If it returns 'Just' @(a,b)@, @a@ is added to the filter and
--     @b@ is used as a new seed.
unfoldB :: (a -> [Hash])        -- ^ family of hash functions to use
        -> Int                  -- ^ number of bits in filter
        -> (b -> Maybe (a, b))  -- ^ seeding function
        -> b                    -- ^ initial seed
        -> Bloom a
unfoldB hashes numBits f k = createB hashes numBits (loop k)
  where loop j mb = case f j of
                      Just (a, j') -> insertMB mb a >> loop j' mb
                      _ -> return ()

-- | Create an immutable Bloom filter, populating it from a list of
-- values.
--
-- Example:
-- @
-- fromListB (cheapHashes 3) 10240 ["foo", "bar", "quux"]
-- @
fromListB :: (a -> [Hash])      -- ^ family of hash functions to use
          -> Int                -- ^ number of bits in filter
          -> [a]                -- ^ values to populate with
          -> Bloom a
fromListB hashes numBits = unfoldB hashes numBits convert
  where convert (x:xs) = Just (x, xs)
        convert _      = Nothing

-- | Slow, crummy way of computing the integer log of an integer known
-- to be a power of two.
logPower2 :: Int -> Int
logPower2 k = go 0 k
    where go j 1 = j
          go j n = go (j+1) (n `shiftR` 1)
