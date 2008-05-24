{-# LANGUAGE ForeignFunctionInterface, TypeOperators #-}

module Data.BloomFilter.Hash
    (
      Hashable(..)
    , hash
    , hashes
    , cheapHashes
    , hashOne
    , hashTwo
    , hashList
    , hashList2
    , hashWord2
    , hashLittle2
    ) where

import Control.Monad (foldM, liftM2)
import Data.Bits ((.&.), shiftL, shiftR, xor)
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
    hashIO :: a -> CInt -> IO CInt
    hashIO2 :: a -> CInt -> CInt -> IO (CInt, CInt)
    hashIO2 v s1 s2 = liftM2 (,) (hashIO v s1) (hashIO v s2)
      
hash :: Hashable a => a -> Word32
hash = hashS 0x106fc397cf62f64d3

hashS :: Hashable a => Word32 -> a -> Word32
hashS salt k =
    let !r = fromIntegral . unsafePerformIO $ hashIO k (fromIntegral salt)
    in r

hashS2 :: Hashable a => Word32 -> Word32 -> a -> (Word32 :* Word32)
{-# SPECIALIZE hashS2 :: Word32 -> Word32 -> SB.ByteString -> (Word32 :* Word32) #-}
hashS2 s1 s2 k =
    unsafePerformIO $ do
      (a, b) <- hashIO2 k (fromIntegral s1) (fromIntegral s2)
      return (fromIntegral a :* fromIntegral b)

hashes :: Hashable a => Int -> a -> [Word32]
hashes n v = unfoldr go (n,0x3f56da2d3ddbb9f631)
    where go (k,s) | k <= 0    = Nothing
                   | otherwise = let s' = hashS s v
                                 in Just (s', (k-1,s'))

-- | Compute a list of hash values using Kirsch and Mitzenmacher's
-- technique.  Any given input is traversed at most twice, regardless
-- of the number of hashes requested.
cheapHashes :: Hashable a => Int -> a -> [Word32]
{-# SPECIALIZE cheapHashes :: Int -> SB.ByteString -> [Word32] #-}
cheapHashes k v = [h1 + (h2 `shiftL` i) | i <- [1..fromIntegral k]]
    where (h1 :* h2) = hashS2 0x3f56da2d3ddbb9f631 0xdc61ab0530200d7554 v

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

alignedHash :: Ptr a -> CSize -> CInt -> IO CInt
alignedHash ptr bytes salt
    | bytes .&. 3 == 0 = hashWord (castPtr ptr) (bytes `shiftR` 2) salt
    | otherwise        = hashLittle ptr bytes salt

alignedHash2 :: Ptr a -> CSize -> CInt -> CInt -> IO (CInt, CInt)
alignedHash2 ptr bytes s1 s2 =
    with s1 $ \p1 ->
        with s2 $ \p2 ->
            go p1 p2 >> liftM2 (,) (peek p1) (peek p2)
  where go p1 p2
          | bytes .&. 3 == 0 = hashWord2 (castPtr ptr) (bytes `shiftR` 2) p1 p2
          | otherwise        = hashLittle2 ptr bytes p1 p2

instance Hashable SB.ByteString where
    hashIO bs salt = SB.useAsCStringLen bs $ \(ptr, len) -> do
                     alignedHash ptr (fromIntegral len) salt
    hashIO2 bs s1 s2 = SB.useAsCStringLen bs $ \(ptr, len) -> do
                       alignedHash2 ptr (fromIntegral len) s1 s2

instance Hashable LB.ByteString where
    hashIO bs salt = foldM (flip hashIO) salt (LB.toChunks bs)
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
    hashIO2 = hashList2

hashOne :: Storable a => a -> CInt -> IO CInt
hashOne k salt = with k $ \ptr ->
                 alignedHash ptr (fromIntegral (sizeOf k)) salt

hashTwo :: Storable a => a -> CInt -> CInt -> IO (CInt, CInt)
hashTwo k s1 s2 = with k $ \ptr ->
                  alignedHash2 ptr (fromIntegral (sizeOf k)) s1 s2

hashList :: Storable a => [a] -> CInt -> IO CInt
hashList xs salt = withArrayLen xs $ \len ptr ->
                   alignedHash ptr (fromIntegral (len * sizeOf (head xs))) salt

hashList2 :: Storable a => [a] -> CInt -> CInt -> IO (CInt, CInt)
hashList2 xs s1 s2 =
    withArrayLen xs $ \len ptr ->
    alignedHash2 ptr (fromIntegral (len * sizeOf (head xs))) s1 s2
