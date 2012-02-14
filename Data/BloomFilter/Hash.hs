{-# LANGUAGE BangPatterns, CPP, ForeignFunctionInterface,
    TypeOperators #-}

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
    , hash32
    , hash64
    , hashSalt32
    , hashSalt64
    -- * Compute a family of hash values
    , hashes
    , cheapHashes
    -- * Hash functions for 'Storable' instances
    , hashOne32
    , hashOne64
    , hashList32
    , hashList64
    ) where

import Control.Monad (foldM)
import Data.Bits ((.&.), (.|.), xor)
import Data.BloomFilter.Util (FastShift(..))
import Data.List (unfoldr)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import Foreign.C.String (CString)
#if __GLASGOW_HASKELL__ >= 704
import Foreign.C.Types (CInt(..), CSize(..))
#else
import Foreign.C.Types (CInt, CSize)
#endif
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray, withArrayLen)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (Storable, peek, poke, sizeOf)
import System.IO.Unsafe (unsafePerformIO)
import Data.ByteString.Internal (ByteString(..))
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy.Internal as LB
import qualified Data.ByteString.Lazy as LB

#include "HsBaseConfig.h"

-- Make sure we're not performing any expensive arithmetic operations.
-- import Prelude hiding ((/), (*), div, divMod, mod, rem)

foreign import ccall unsafe "lookup3.h _jenkins_hashword" hashWord
    :: Ptr Word32 -> CSize -> Word32 -> IO Word32

foreign import ccall unsafe "lookup3.h _jenkins_hashword2" hashWord2
    :: Ptr Word32 -> CSize -> Ptr Word32 -> Ptr Word32 -> IO ()

foreign import ccall unsafe "lookup3.h _jenkins_hashlittle" hashLittle
    :: Ptr a -> CSize -> Word32 -> IO Word32

foreign import ccall unsafe "lookup3.h _jenkins_hashlittle2" hashLittle2
    :: Ptr a -> CSize -> Ptr Word32 -> Ptr Word32 -> IO ()

class Hashable a where
    -- | Compute a 32-bit hash of a value.  The salt value perturbs
    -- the result.
    hashIO32 :: a               -- ^ value to hash
             -> Word32          -- ^ salt
             -> IO Word32

    -- | Compute a 64-bit hash of a value.  The first salt value
    -- perturbs the first element of the result, and the second salt
    -- perturbs the second.
    hashIO64 :: a               -- ^ value to hash
             -> Word64           -- ^ salt
             -> IO Word64
    hashIO64 v salt = do
                   let s1 = fromIntegral (salt `shiftR` 32) .&. maxBound
                       s2 = fromIntegral salt
                   h1 <- hashIO32 v s1
                   h2 <- hashIO32 v s2
                   return $ (fromIntegral h1 `shiftL` 32) .|. fromIntegral h2
      
-- | Compute a 32-bit hash.
hash32 :: Hashable a => a -> Word32
hash32 = hashSalt32 0x106fc397

hash64 :: Hashable a => a -> Word64
hash64 = hashSalt64 0x106fc397cf62f64d3

-- | Compute a salted 32-bit hash.
hashSalt32 :: Hashable a => Word32  -- ^ salt
           -> a                 -- ^ value to hash
           -> Word32
{-# INLINE hashSalt32 #-}
hashSalt32 salt k = unsafePerformIO $ hashIO32 k salt

-- | Compute a salted 64-bit hash.
hashSalt64 :: Hashable a => Word64  -- ^ salt
           -> a                 -- ^ value to hash
           -> Word64
{-# INLINE hashSalt64 #-}
hashSalt64 salt k = unsafePerformIO $ hashIO64 k salt

-- | Compute a list of 32-bit hashes.  The value to hash may be
-- inspected as many times as there are hashes requested.
hashes :: Hashable a => Int     -- ^ number of hashes to compute
       -> a                     -- ^ value to hash
       -> [Word32]
hashes n v = unfoldr go (n,0x3f56da2d3ddbb9f6)
    where go (k,s) | k <= 0    = Nothing
                   | otherwise = let s' = hashSalt32 s v
                                 in Just (s', (k-1,s'))

-- | Compute a list of 32-bit hashes relatively cheaply.  The value to
-- hash is inspected at most twice, regardless of the number of hashes
-- requested.
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
cheapHashes k v = go 0
    where go i | i == j = []
               | otherwise = hash : go (i + 1)
               where !hash = h1 + (h2 `shiftR` i)
          h1 = fromIntegral (h `shiftR` 32)
          h2 = fromIntegral h
          h = hashSalt64 0x9150a946c4a8966e v
          j = fromIntegral k

instance Hashable () where
    hashIO32 _ salt = return salt

instance Hashable Integer where
    hashIO32 k salt | k < 0 = hashIO32 (unfoldr go (-k))
                                   (salt `xor` 0x3ece731e9c1c64f8)
                  | otherwise = hashIO32 (unfoldr go k) salt
        where go 0 = Nothing
              go i = Just (fromIntegral i :: Word32, i `shiftR` 32)

instance Hashable Bool where
    hashIO32 = hashOne32
    hashIO64 = hashOne64

instance Hashable Ordering where
    hashIO32 = hashIO32 . fromEnum
    hashIO64 = hashIO64 . fromEnum

instance Hashable Char where
    hashIO32 = hashOne32
    hashIO64 = hashOne64

instance Hashable Int where
    hashIO32 = hashOne32
    hashIO64 = hashOne64

instance Hashable Float where
    hashIO32 = hashOne32
    hashIO64 = hashOne64

instance Hashable Double where
    hashIO32 = hashOne32
    hashIO64 = hashOne64

instance Hashable Int8 where
    hashIO32 = hashOne32
    hashIO64 = hashOne64

instance Hashable Int16 where
    hashIO32 = hashOne32
    hashIO64 = hashOne64

instance Hashable Int32 where
    hashIO32 = hashOne32
    hashIO64 = hashOne64

instance Hashable Int64 where
    hashIO32 = hashOne32
    hashIO64 = hashOne64

instance Hashable Word8 where
    hashIO32 = hashOne32
    hashIO64 = hashOne64

instance Hashable Word16 where
    hashIO32 = hashOne32
    hashIO64 = hashOne64

instance Hashable Word32 where
    hashIO32 = hashOne32
    hashIO64 = hashOne64

instance Hashable Word64 where
    hashIO32 = hashOne32
    hashIO64 = hashOne64

-- | A fast unchecked shift.  Nasty, but otherwise GHC 6.8.2 does a
-- test and branch on every shift.
div4 :: CSize -> CSize
div4 k = fromIntegral ((fromIntegral k :: HTYPE_SIZE_T) `shiftR` 2)

alignedHash :: Ptr a -> CSize -> Word32 -> IO Word32
alignedHash ptr bytes salt
    | bytes .&. 3 == 0 = hashWord (castPtr ptr) (div4 bytes) salt'
    | otherwise        = hashLittle ptr bytes salt'
  where salt' = fromIntegral salt

-- Inlined from Foreign.Marshal.Utils, for performance reasons.
with :: Storable a => a -> (Ptr a -> IO b) -> IO b
with val f  =
  alloca $ \ptr -> do
    poke ptr val
    f ptr

alignedHash2 :: Ptr a -> CSize -> Word64 -> IO Word64
alignedHash2 ptr bytes salt =
    with (fromIntegral salt) $ \sp -> do
      let p1 = castPtr sp
          p2 = castPtr sp `plusPtr` 4
      doubleHash ptr bytes p1 p2
      peek sp

doubleHash :: Ptr a -> CSize -> Ptr Word32 -> Ptr Word32 -> IO ()
doubleHash ptr bytes p1 p2
          | bytes .&. 3 == 0 = hashWord2 (castPtr ptr) (div4 bytes) p1 p2
          | otherwise        = hashLittle2 ptr bytes p1 p2

instance Hashable SB.ByteString where
    hashIO32 bs salt = unsafeUseAsCStringLen bs $ \ptr len ->
                       alignedHash ptr (fromIntegral len) salt

    {-# INLINE hashIO64 #-}
    hashIO64 bs salt = unsafeUseAsCStringLen bs $ \ptr len ->
                       alignedHash2 ptr (fromIntegral len) salt

rechunk :: LB.ByteString -> [SB.ByteString]
rechunk s | LB.null s = []
          | otherwise = let (pre,suf) = LB.splitAt chunkSize s
                        in  repack pre : rechunk suf
    where repack    = SB.concat . LB.toChunks
          chunkSize = fromIntegral LB.defaultChunkSize

instance Hashable LB.ByteString where
    hashIO32 bs salt = foldM (flip hashIO32) salt (rechunk bs)

    {-# INLINE hashIO64 #-}
    hashIO64 = hashChunks

instance Hashable a => Hashable (Maybe a) where
    hashIO32 Nothing salt = return salt
    hashIO32 (Just k) salt = hashIO32 k salt
    hashIO64 Nothing salt = return salt
    hashIO64 (Just k) salt = hashIO64 k salt

instance (Hashable a, Hashable b) => Hashable (Either a b) where
    hashIO32 (Left a) salt = hashIO32 a salt
    hashIO32 (Right b) salt = hashIO32 b (salt + 1)
    hashIO64 (Left a) salt = hashIO64 a salt
    hashIO64 (Right b) salt = hashIO64 b (salt + 1)

instance (Hashable a, Hashable b) => Hashable (a, b) where
    hashIO32 (a,b) salt = hashIO32 a salt >>= hashIO32 b
    hashIO64 (a,b) salt = hashIO64 a salt >>= hashIO64 b

instance (Hashable a, Hashable b, Hashable c) => Hashable (a, b, c) where
    hashIO32 (a,b,c) salt = hashIO32 a salt >>= hashIO32 b >>= hashIO32 c

instance (Hashable a, Hashable b, Hashable c, Hashable d) =>
    Hashable (a, b, c, d) where
    hashIO32 (a,b,c,d) salt =
        hashIO32 a salt >>= hashIO32 b >>= hashIO32 c >>= hashIO32 d

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e) =>
    Hashable (a, b, c, d, e) where
    hashIO32 (a,b,c,d,e) salt =
        hashIO32 a salt >>= hashIO32 b >>= hashIO32 c >>= hashIO32 d >>= hashIO32 e

instance Storable a => Hashable [a] where
    hashIO32 = hashList32

    {-# INLINE hashIO64 #-}
    hashIO64 = hashList64

-- | Compute a 32-bit hash of a 'Storable' instance.
hashOne32 :: Storable a => a -> Word32 -> IO Word32
hashOne32 k salt = with k $ \ptr ->
                 alignedHash ptr (fromIntegral (sizeOf k)) salt

-- | Compute a 64-bit hash of a 'Storable' instance.
hashOne64 :: Storable a => a -> Word64 -> IO Word64
hashOne64 k salt = with k $ \ptr ->
                   alignedHash2 ptr (fromIntegral (sizeOf k)) salt

-- | Compute a 32-bit hash of a list of 'Storable' instances.
hashList32 :: Storable a => [a] -> Word32 -> IO Word32
hashList32 xs salt =
    withArrayLen xs $ \len ptr ->
        alignedHash ptr (fromIntegral (len * sizeOf (head xs))) salt

-- | Compute a 64-bit hash of a list of 'Storable' instances.
hashList64 :: Storable a => [a] -> Word64 -> IO Word64
hashList64 xs salt =
    withArrayLen xs $ \len ptr ->
        alignedHash2 ptr (fromIntegral (len * sizeOf (head xs))) salt

unsafeUseAsCStringLen :: SB.ByteString -> (CString -> Int -> IO a) -> IO a
unsafeUseAsCStringLen (PS fp o l) action =
    withForeignPtr fp $ \p -> action (p `plusPtr` o) l

type HashState = Ptr Word32

foreign import ccall unsafe "lookup3.h _jenkins_little2_begin" c_begin
    :: Ptr Word32 -> Ptr Word32 -> HashState -> IO ()

foreign import ccall unsafe "lookup3.h _jenkins_little2_frag" c_frag
    :: Ptr a -> CSize -> HashState -> CSize -> IO CSize

foreign import ccall unsafe "lookup3.h _jenkins_little2_step" c_step
    :: Ptr a -> CSize -> HashState -> IO CSize

foreign import ccall unsafe "lookup3.h _jenkins_little2_end" c_end
    :: CInt -> Ptr Word32 -> Ptr Word32 -> HashState -> IO ()

unsafeAdjustCStringLen :: SB.ByteString -> Int -> (CString -> Int -> IO a)
                       -> IO a
unsafeAdjustCStringLen (PS fp o l) d action
  | d > l     = action nullPtr 0
  | otherwise = withForeignPtr fp $ \p -> action (p `plusPtr` (o + d)) (l - d)

hashChunks :: LB.ByteString -> Word64 -> IO Word64
hashChunks s salt = do
    with (fromIntegral salt) $ \sp -> do
      let p1 = castPtr sp
          p2 = castPtr sp `plusPtr` 4
      allocaArray 3 $ \st -> do
        let step :: LB.ByteString -> Int -> IO Int
            step (LB.Chunk x xs) off = do
              unread <- unsafeAdjustCStringLen x off $ \ptr len ->
                        c_step ptr (fromIntegral len) st
              if unread > 0
                then frag xs unread
                else step xs 0
            step _ _ = return 0

            frag :: LB.ByteString -> CSize -> IO Int
            frag c@(LB.Chunk x xs) stoff = do
              nstoff <- unsafeUseAsCStringLen x $ \ptr len -> do
                c_frag ptr (fromIntegral len) st stoff
              if nstoff == 12
                then step c (fromIntegral (nstoff - stoff))
                else frag xs nstoff
            frag LB.Empty stoff = return (fromIntegral (12 - stoff))
        c_begin p1 p2 st 
        unread <- step s 0
        c_end (fromIntegral unread) p1 p2 st
      peek sp
