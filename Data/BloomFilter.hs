{-# LANGUAGE TypeOperators #-}

module Data.BloomFilter
    (
      Hash

    , UBloom
    , unfoldUB
    , fromListUB

    , lengthUB
    , elemUB
    , bitsUB

    , MBloom
    , newMB
    , lengthMB
    , insertMB
    , lookupMB
    , bitsMB
    , unsafeFreezeMB

    , MaybeS(..)
    , (:*:)(..)
    ) where

import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Data.Array.Vector
import Data.Bits ((.&.), (.|.), shiftL)
import Data.Word (Word32)
import Foreign.Storable (sizeOf)

type Hash = Word32

data MBloom a s = MB {
      hashMB :: a -> [Hash]
    , bitsMB :: {-# UNPACK #-} !(MUArr Hash s)
    }

data UBloom a = UB {
      hashUB :: a -> [Hash]
    , bitsUB :: {-# UNPACK #-} !(UArr Hash)
    }

instance Show (MBloom a s) where
    show mb = "MBloom { " ++ show (lengthMB mb) ++ " bits } "

instance Show (UBloom a) where
    show ub = "UBloom { " ++ show (lengthUB ub) ++ " bits } "

newMB :: (a -> [Hash]) -> Int -> ST s (MBloom a s)
newMB hash numBits = do
    mu <- newMU numElems
    zeroFill mu 0
    return (MB hash mu)
  where numElems = case bitsToLength numBits of
                     1 -> 2
                     n -> n
        zeroFill mu n | n == numElems = return ()
                      | otherwise     = writeMU mu n 0 >> zeroFill mu (n + 1)

dm :: Int -> Int -> (Int :*: Int)
dm x y = (x `div` y) :*: (x `mod` y)

hashesM :: MBloom a s -> a -> [(Int :*: Int)]
hashesM mb elt = map go (hashMB mb elt)
    where go k = (fromIntegral k `mod` lengthMB mb) `dm` bitsInHash

hashesU :: UBloom a -> a -> [(Int :*: Int)]
hashesU ub elt = map go (hashUB ub elt)
    where go k = (fromIntegral k `mod` lengthUB ub) `dm` bitsInHash

insertMB :: MBloom a s -> a -> ST s ()
{-# INLINE insertMB #-}
insertMB mb elt = do
  let mu = bitsMB mb
  forM_ (hashesM mb elt) $ \(word :*: bit) -> do
      old <- readMU mu word
      writeMU mu word (old .|. (1 `shiftL` bit))

lookupMB :: MBloom a s -> a -> ST s Bool
{-# INLINE lookupMB #-}
lookupMB mb elt = loop (hashesM mb elt)
  where mu = bitsMB mb
        loop ((word :*: bit):wbs) = do
          i <- readMU mu word
          if i .&. (1 `shiftL` bit) /= 0
            then return True
            else loop wbs
        loop _ = return False

elemUB :: a -> UBloom a -> Bool
{-# INLINE elemUB #-}
elemUB elt ub = any test (hashesU ub elt)
  where test (off :*: bit) = (ua `indexU` off) .&. (1 `shiftL` bit) /= 0
        ua = bitsUB ub
          
unsafeFreezeMB :: MBloom a s -> ST s (UBloom a)
{-# INLINE unsafeFreezeMB #-}
unsafeFreezeMB mb = do
  u <- unsafeFreezeAllMU (bitsMB mb)
  return (UB (hashMB mb) u)

bitsInHash :: Int
bitsInHash = sizeOf (undefined :: Hash) * 8

bitsToLength :: Int -> Int
bitsToLength numBits = ((numBits - 1) `div` bitsInHash) + 1

lengthMB :: MBloom a s -> Int
{-# INLINE lengthMB #-}
lengthMB mb = bitsInHash * lengthMU (bitsMB mb)

lengthUB :: UBloom a -> Int
{-# INLINE lengthUB #-}
lengthUB mb = bitsInHash * lengthU (bitsUB mb)

unfoldUB :: (a -> [Hash]) -> Int -> (b -> MaybeS (a :*: b)) -> b -> UBloom a
{-# INLINE unfoldUB #-}
unfoldUB hashes numBits f k =
    runST $ do
      mb <- newMB hashes numBits
      loop mb k
      unsafeFreezeMB mb
  where loop mb j = case f j of
                      JustS (a :*: j') -> insertMB mb a >> loop mb j'
                      _ -> return ()

fromListUB :: (a -> [Hash]) -> Int -> [a] -> UBloom a
{-# INLINE fromListUB #-}
fromListUB hashes numBits = unfoldUB hashes numBits convert
  where convert (x:xs) = JustS (x :*: xs)
        convert _ = NothingS
