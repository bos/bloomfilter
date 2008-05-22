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
import Data.Array.ST
import Data.Array.Unboxed (UArray, (!), bounds)
import Data.Bits ((.&.), (.|.), shiftL)
import Data.Word (Word32)
import Foreign.Storable (sizeOf)

type Hash = Word32

-- | Mutable Bloom filter, for use within the 'ST' monad.
data MBloom s a = MB {
      hashMB :: {-# UNPACK #-} !(a -> [Hash])
    , bitArrayMB :: {-# UNPACK #-} !(STUArray s Int Hash)
    }

-- | Immutable Bloom filter, suitable for querying in pure code.
data Bloom a = B {
      hashB :: {-# UNPACK #-} !(a -> [Hash])
    , bitArrayB :: {-# UNPACK #-} !(UArray Int Hash)
    }

instance Show (MBloom s a) where
    show mb = "MBloom { " ++ show (lengthMB mb) ++ " bits } "

instance Show (Bloom a) where
    show ub = "Bloom { " ++ show (lengthB ub) ++ " bits } "

-- | Create a new mutable Bloom filter.  For a safer creation
-- interface, use 'createB'.  To return a mutable filter to pure code
-- as an immutable filter, use 'unsafeFreezeMB'.
newMB :: (a -> [Hash])          -- ^ family of hash functions to use
      -> Int                    -- ^ number of bits in filter
      -> ST s (MBloom s a)
newMB hash numBits = do
    mu <- newArray (0,numElems-1) 0
    return (MB hash mu)
  where numElems = case bitsToLength numBits of
                     1 -> 2
                     n -> n

-- | Create an immutable Bloom filter, using the given setup function.
createB :: (a -> [Hash])        -- ^ family of hash functions to use
        -> Int                  -- ^ number of bits in filter
        -> (forall s. (MBloom s a -> ST s ()))  -- ^ setup function
        -> Bloom a
{-# INLINE createB #-}
createB hash numBits body = runST $ do
  mb <- newMB hash numBits
  body mb
  unsafeFreezeMB mb

-- | An unboxed, strict pair type.
data a :* b = {-# UNPACK #-} !a :* {-# UNPACK #-} !b

dm :: Int -> Int -> (Int :* Int)
dm x y = (x `div` y) :* (x `mod` y)

-- | Hash the given value, returning a list of (word offset, bit
-- offset) pairs, one per hash value.
hashesM :: MBloom s a -> a -> ST s [Int :* Int]
hashesM mb elt = do
  len <- lengthMB mb
  let go k = (fromIntegral k `mod` len) `dm` bitsInHash
  return . map go $ hashMB mb elt

-- | Hash the given value, returning a list of (word offset, bit
-- offset) pairs, one per hash value.
hashesU :: Bloom a -> a -> [Int :* Int]
hashesU ub elt = map go (hashB ub elt)
    where go k = (fromIntegral k `mod` lengthB ub) `dm` bitsInHash

-- | Insert a value into a mutable Bloom filter.  Afterwards, a
-- membership query for the same value is guaranteed to return @True@.
insertMB :: MBloom s a -> a -> ST s ()
{-# INLINE insertMB #-}
insertMB mb elt = do
  let mu = bitArrayMB mb
  hashes <- hashesM mb elt
  forM_ hashes $ \(word :* bit) -> do
      old <- readArray mu word
      writeArray mu word (old .|. (1 `shiftL` bit))

-- | Query a mutable Bloom filter for membership.  If the value is
-- present, return @True@.  If the value is not present, there is
-- /still/ some possibility that @True@ will be returned.
elemMB :: a -> MBloom s a -> ST s Bool
{-# INLINE elemMB #-}
elemMB elt mb = hashesM mb elt >>= loop
  where mu = bitArrayMB mb
        loop ((word :* bit):wbs) = do
          i <- readArray mu word
          if i .&. (1 `shiftL` bit) /= 0
            then return True
            else loop wbs
        loop _ = return False

-- | Query an immutable Bloom filter for membership.  If the value is
-- present, return @True@.  If the value is not present, there is
-- /still/ some possibility that @True@ will be returned.
elemB :: a -> Bloom a -> Bool
{-# INLINE elemB #-}
elemB elt ub = any test (hashesU ub elt)
  where test (off :* bit) = (bitArrayB ub ! off) .&. (1 `shiftL` bit) /= 0
          
-- | Create an immutable Bloom filter from a mutable one.  The mutable
-- filter /must not/ be modified afterwards, or a runtime crash may
-- occur.  For a safer creation interface, use 'createB'.
unsafeFreezeMB :: MBloom s a -> ST s (Bloom a)
{-# INLINE unsafeFreezeMB #-}
unsafeFreezeMB mb = B (hashMB mb) `liftM` unsafeFreeze (bitArrayMB mb)

-- | Copy an immutable Bloom filter to create a mutable one.  There is
-- no non-copying equivalent.
thawMB :: Bloom a -> ST s (MBloom s a)
thawMB ub = MB (hashB ub) `liftM` thaw (bitArrayB ub)

bitsInHash :: Int
bitsInHash = sizeOf (undefined :: Hash) * 8

bitsToLength :: Int -> Int
bitsToLength numBits = ((numBits - 1) `div` bitsInHash) + 1

countBits :: (Int, Int) -> Int
countBits = (bitsInHash *) . (1+) . snd

-- | Return the number of bits in this mutable Bloom filter.
lengthMB :: MBloom s a -> ST s Int
{-# INLINE lengthMB #-}
lengthMB mb = countBits `liftM` getBounds (bitArrayMB mb)

-- | Return the number of bits in this immutable Bloom filter.
lengthB :: Bloom a -> Int
{-# INLINE lengthB #-}
lengthB = countBits . bounds . bitArrayB

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
{-# INLINE unfoldB #-}
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
          -> [a]                -- ^ list of values to add
          -> Bloom a
{-# INLINE fromListB #-}
fromListB hashes numBits = unfoldB hashes numBits convert
  where convert (x:xs) = Just (x, xs)
        convert _      = Nothing
