{-# LANGUAGE Rank2Types #-}

module Data.BloomFilter
    (
      Hash

    , UBloom
    , unfoldUB
    , fromListUB
    , createUB

    , lengthUB
    , elemUB

    , MBloom
    , newMB
    , lengthMB
    , insertMB
    , elemMB
    , unsafeFreezeMB

    , bitArrayUB
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

data MBloom s a = MB {
      hashMB :: {-# UNPACK #-} !(a -> [Hash])
    , bitArrayMB :: {-# UNPACK #-} !(STUArray s Int Hash)
    }

data UBloom a = UB {
      hashUB :: {-# UNPACK #-} !(a -> [Hash])
    , bitArrayUB :: {-# UNPACK #-} !(UArray Int Hash)
    }

instance Show (MBloom s a) where
    show mb = "MBloom { " ++ show (lengthMB mb) ++ " bits } "

instance Show (UBloom a) where
    show ub = "UBloom { " ++ show (lengthUB ub) ++ " bits } "

newMB :: (a -> [Hash]) -> Int -> ST s (MBloom s a)
newMB hash numBits = do
    mu <- newArray (0,numElems-1) 0
    return (MB hash mu)
  where numElems = case bitsToLength numBits of
                     1 -> 2
                     n -> n

createUB :: (a -> [Hash]) -> Int
         -> (forall s. (MBloom s a -> ST s ())) -> UBloom a
{-# INLINE createUB #-}
createUB hash numBits body = runST $ do
  mb <- newMB hash numBits
  body mb
  unsafeFreezeMB mb

hashesM :: MBloom s a -> a -> ST s [(Int, Int)]
hashesM mb elt = do
  len <- lengthMB mb
  let go k = (fromIntegral k `mod` len) `divMod` bitsInHash
  return . map go $ hashMB mb elt

hashesU :: UBloom a -> a -> [(Int, Int)]
hashesU ub elt = map go (hashUB ub elt)
    where go k = (fromIntegral k `mod` lengthUB ub) `divMod` bitsInHash

insertMB :: MBloom s a -> a -> ST s ()
{-# INLINE insertMB #-}
insertMB mb elt = do
  let mu = bitArrayMB mb
  hashes <- hashesM mb elt
  forM_ hashes $ \(word, bit) -> do
      old <- readArray mu word
      writeArray mu word (old .|. (1 `shiftL` bit))

elemMB :: a -> MBloom s a -> ST s Bool
{-# INLINE elemMB #-}
elemMB elt mb = hashesM mb elt >>= loop
  where mu = bitArrayMB mb
        loop ((word, bit):wbs) = do
          i <- readArray mu word
          if i .&. (1 `shiftL` bit) /= 0
            then return True
            else loop wbs
        loop _ = return False

elemUB :: a -> UBloom a -> Bool
{-# INLINE elemUB #-}
elemUB elt ub = any test (hashesU ub elt)
  where test (off, bit) = (bitArrayUB ub ! off) .&. (1 `shiftL` bit) /= 0
          
unsafeFreezeMB :: MBloom s a -> ST s (UBloom a)
{-# INLINE unsafeFreezeMB #-}
unsafeFreezeMB mb = UB (hashMB mb) `liftM` unsafeFreeze (bitArrayMB mb)

bitsInHash :: Int
bitsInHash = sizeOf (undefined :: Hash) * 8

bitsToLength :: Int -> Int
bitsToLength numBits = ((numBits - 1) `div` bitsInHash) + 1

countBits :: (Int, Int) -> Int
countBits = (bitsInHash *) . (1+) . snd

lengthMB :: MBloom s a -> ST s Int
{-# INLINE lengthMB #-}
lengthMB mb = countBits `liftM` getBounds (bitArrayMB mb)

lengthUB :: UBloom a -> Int
{-# INLINE lengthUB #-}
lengthUB = countBits . bounds . bitArrayUB

unfoldUB :: (a -> [Hash]) -> Int -> (b -> Maybe (a, b)) -> b -> UBloom a
{-# INLINE unfoldUB #-}
unfoldUB hashes numBits f k = createUB hashes numBits (loop k)
  where loop j mb = case f j of
                      Just (a, j') -> insertMB mb a >> loop j' mb
                      _ -> return ()

fromListUB :: (a -> [Hash]) -> Int -> [a] -> UBloom a
{-# INLINE fromListUB #-}
fromListUB hashes numBits = unfoldUB hashes numBits convert
  where convert (x:xs) = Just (x, xs)
        convert _      = Nothing
