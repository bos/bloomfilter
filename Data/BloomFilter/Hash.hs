{-# LANGUAGE CPP, ForeignFunctionInterface, TypeOperators #-}

-- |
-- Module: Data.BloomFilter.Hash
-- Copyright: Bryan O'Sullivan
-- License: BSD3
--
-- Maintainer: Bryan O'Sullivan <bos@serpentine.com>
-- Stability: unstable
-- Portability: portable
--
-- Fast hashing of Haskell values.  The hash functions used are Bob
-- Jenkins's public domain functions, which combine high performance
-- with excellent mixing properties.  For more details, see
-- <http://burtleburtle.net/bob/hash/>.
--
-- In addition to the usual "one input, one output" hash functions,
-- this module provides multi-output hash functions, suitable for use
-- in applications that need multiple hashes, such as Bloom filtering.

module Data.BloomFilter.Hash
    (
    -- * Basic hash functionality
      Hashable(..)
    , hash
    -- * Compute a family of hash values
    , hashes
    , cheapHashes
    -- * Hash functions for 'Storable' instances
    , hashOne
    , hashTwo
    , hashList
    , hashList2
    ) where

import Control.Monad (foldM, liftM2)
import Data.Bits ((.&.), xor)
import Data.BloomFilter.Util
import Data.List (unfoldr)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import Foreign.C.Types (CInt, CSize)
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable, peek, sizeOf)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB

#include "HsBaseConfig.h"

-- Make sure we're not performing any expensive arithmetic operations.
-- import Prelude hiding ((/), (*), div, divMod, mod, rem)

foreign import ccall unsafe "_jenkins_hashword" hashWord
    :: Ptr CInt -> CSize -> CInt -> IO CInt

foreign import ccall unsafe "_jenkins_hashword2" hashWord2
    :: Ptr CInt -> CSize -> Ptr CInt -> Ptr CInt -> IO ()

foreign import ccall unsafe "_jenkins_hashlittle" hashLittle
    :: Ptr a -> CSize -> CInt -> IO CInt

foreign import ccall unsafe "_jenkins_hashlittle2" hashLittle2
    :: Ptr a -> CSize -> Ptr CInt -> Ptr CInt -> IO ()

class Hashable a where
    -- | Compute a single hash of a value.  The salt value perturbs
    -- the result.
    hashIO :: a                 -- ^ value to hash
           -> CInt              -- ^ salt value
           -> IO CInt

    -- | Compute two hashes of a value.  The first salt value perturbs
    -- the first element of the result, and the second salt perturbs
    -- the second.
    hashIO2 :: a                -- ^ value to hash
            -> CInt             -- ^ first salt value
            -> CInt             -- ^ second salt value
            -> IO (CInt, CInt)
    hashIO2 v s1 s2 = liftM2 (,) (hashIO v s1) (hashIO v s2)
      
-- | Compute a hash.
hash :: Hashable a => a -> Word32
hash = hashS 0x106fc397cf62f64d3

hashS :: Hashable a => Word32 -> a -> Word32
hashS salt k =
    let !r = fromIntegral . unsafePerformIO $ hashIO k (fromIntegral salt)
    in r

hashS2 :: Hashable a => Word32 -> Word32 -> a -> (Word32 :* Word32)
{-# INLINE hashS2 #-}
hashS2 s1 s2 k =
    unsafePerformIO $ do
      (a, b) <- hashIO2 k (fromIntegral s1) (fromIntegral s2)
      return (fromIntegral a :* fromIntegral b)

-- | Compute a list of hashes.  The value to hash may be inspected as
-- many times as there are hashes requested.
hashes :: Hashable a => Int     -- ^ number of hashes to compute
       -> a                     -- ^ value to hash
       -> [Word32]
hashes n v = unfoldr go (n,0x3f56da2d3ddbb9f631)
    where go (k,s) | k <= 0    = Nothing
                   | otherwise = let s' = hashS s v
                                 in Just (s', (k-1,s'))

-- | Compute a list of hashes relatively cheaply.
-- The value to hash is inspected at most twice, regardless of the
-- number of hashes requested.
--
-- We use a variant of Kirsch and Mitzenmacher's technique from \"Less
-- Hashing, Same Performance: Building a Better Bloom Filter\",
-- <http://www.eecs.harvard.edu/~kirsch/pubs/bbbf/esa06.pdf>.
--
-- Where Kirsch and Mitzenmacher multiply the second hash by a
-- coefficient, we shift right by the coefficient.  This offers better
-- performance (as a shift is much cheaper than a multiply), and the
-- low order bits of the final hash stay well mixed.
cheapHashes :: Hashable a => Int -- ^ number of hashes to compute
            -> a                 -- ^ value to hash
            -> [Word32]
{-# SPECIALIZE cheapHashes :: Int -> SB.ByteString -> [Word32] #-}
{-# SPECIALIZE cheapHashes :: Int -> LB.ByteString -> [Word32] #-}
{-# SPECIALIZE cheapHashes :: Int -> String -> [Word32] #-}
cheapHashes k v = [h1 + (h2 `shiftR` i) | i <- [0..j]]
    where (h1 :* h2) = hashS2 0x3f56da2d3ddbb9f631 0xdc61ab0530200d7554 v
          j = fromIntegral k - 1

instance Hashable () where
    hashIO _ salt = return salt

instance Hashable Integer where
    hashIO k salt | k < 0 = hashIO (unfoldr go (-k))
                                   (salt `xor` 0x3ece731e9c1c64f8)
                  | otherwise = hashIO (unfoldr go k) salt
        where go 0 = Nothing
              go i = Just (fromIntegral i :: Word32, i `shiftR` 32)

instance Hashable Bool where
    hashIO = hashOne
    hashIO2 = hashTwo

instance Hashable Ordering where
    hashIO = hashIO . fromEnum
    hashIO2 = hashIO2 . fromEnum

instance Hashable Char where
    hashIO = hashOne
    hashIO2 = hashTwo

instance Hashable Int where
    hashIO = hashOne
    hashIO2 = hashTwo

instance Hashable Float where
    hashIO = hashOne
    hashIO2 = hashTwo

instance Hashable Double where
    hashIO = hashOne
    hashIO2 = hashTwo

instance Hashable Int8 where
    hashIO = hashOne
    hashIO2 = hashTwo

instance Hashable Int16 where
    hashIO = hashOne
    hashIO2 = hashTwo

instance Hashable Int32 where
    hashIO = hashOne
    hashIO2 = hashTwo

instance Hashable Int64 where
    hashIO = hashOne
    hashIO2 = hashTwo

instance Hashable Word8 where
    hashIO = hashOne
    hashIO2 = hashTwo

instance Hashable Word16 where
    hashIO = hashOne
    hashIO2 = hashTwo

instance Hashable Word32 where
    hashIO = hashOne
    hashIO2 = hashTwo

instance Hashable Word64 where
    hashIO = hashOne
    hashIO2 = hashTwo

-- | A fast unchecked shift.  Nasty, but otherwise GHC 6.8.2 does a
-- test and branch on every shift.
div4 :: CSize -> CSize
div4 k = fromIntegral ((fromIntegral k :: HTYPE_SIZE_T) `shiftR` 2)

alignedHash :: Ptr a -> CSize -> CInt -> IO CInt
alignedHash ptr bytes salt
    | bytes .&. 3 == 0 = hashWord (castPtr ptr) (div4 bytes) salt
    | otherwise        = hashLittle ptr bytes salt

alignedHash2 :: Ptr a -> CSize -> CInt -> CInt -> IO (CInt, CInt)
alignedHash2 ptr bytes s1 s2 =
    with s1 $ \p1 ->
        with s2 $ \p2 ->
            go p1 p2 >> liftM2 (,) (peek p1) (peek p2)
  where go p1 p2
          | bytes .&. 3 == 0 = hashWord2 (castPtr ptr) (div4 bytes) p1 p2
          | otherwise        = hashLittle2 ptr bytes p1 p2

instance Hashable SB.ByteString where
    hashIO bs salt = SB.useAsCStringLen bs $ \(ptr, len) -> do
                     alignedHash ptr (fromIntegral len) salt

    {-# INLINE hashIO2 #-}
    hashIO2 bs s1 s2 = SB.useAsCStringLen bs $ \(ptr, len) -> do
                       alignedHash2 ptr (fromIntegral len) s1 s2

instance Hashable LB.ByteString where
    hashIO bs salt = foldM (flip hashIO) salt (LB.toChunks bs)

    {-# INLINE hashIO2 #-}
    hashIO2 bs s1 s2 = foldM go (s1, s2) (LB.toChunks bs)
        where go (a, b) s = hashIO2 s a b

instance Hashable a => Hashable (Maybe a) where
    hashIO Nothing salt = return salt
    hashIO (Just k) salt = hashIO k salt
    hashIO2 Nothing s1 s2 = return (s1, s2)
    hashIO2 (Just k) s1 s2 = hashIO2 k s1 s2

instance (Hashable a, Hashable b) => Hashable (Either a b) where
    hashIO (Left a) salt = hashIO a salt
    hashIO (Right b) salt = hashIO b (salt + 1)
    hashIO2 (Left a) s1 s2 = hashIO2 a s1 s2
    hashIO2 (Right b) s1 s2 = hashIO2 b (s1 + 1) (s2 + 1)

instance (Hashable a, Hashable b) => Hashable (a, b) where
    hashIO (a,b) salt = hashIO a salt >>= hashIO b
    hashIO2 (a,b) s1 s2 = hashIO2 a s1 s2 >>= uncurry (hashIO2 b)

instance (Hashable a, Hashable b, Hashable c) => Hashable (a, b, c) where
    hashIO (a,b,c) salt = hashIO a salt >>= hashIO b >>= hashIO c

instance (Hashable a, Hashable b, Hashable c, Hashable d) =>
    Hashable (a, b, c, d) where
    hashIO (a,b,c,d) salt =
        hashIO a salt >>= hashIO b >>= hashIO c >>= hashIO d

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e) =>
    Hashable (a, b, c, d, e) where
    hashIO (a,b,c,d,e) salt =
        hashIO a salt >>= hashIO b >>= hashIO c >>= hashIO d >>= hashIO e

instance Storable a => Hashable [a] where
    hashIO = hashList

    {-# INLINE hashIO2 #-}
    hashIO2 = hashList2

-- | Compute a hash of a 'Storable' instance.
hashOne :: Storable a => a -> CInt -> IO CInt
hashOne k salt = with k $ \ptr ->
                 alignedHash ptr (fromIntegral (sizeOf k)) salt

-- | Compute two hashes of a 'Storable' instance.
hashTwo :: Storable a => a -> CInt -> CInt -> IO (CInt, CInt)
hashTwo k s1 s2 = with k $ \ptr ->
                  alignedHash2 ptr (fromIntegral (sizeOf k)) s1 s2

-- | Compute a hash of a list of 'Storable' instances.
hashList :: Storable a => [a] -> CInt -> IO CInt
hashList xs salt = withArrayLen xs $ \len ptr ->
                   alignedHash ptr (fromIntegral (len * sizeOf (head xs))) salt

-- | Compute two hashes of a list of 'Storable' instances.
hashList2 :: Storable a => [a] -> CInt -> CInt -> IO (CInt, CInt)
hashList2 xs s1 s2 =
    withArrayLen xs $ \len ptr ->
    alignedHash2 ptr (fromIntegral (len * sizeOf (head xs))) s1 s2
