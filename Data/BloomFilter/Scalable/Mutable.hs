-- |
-- Module: Data.BloomFilter.Scalable.Mutable
-- Copyright: Bryan O'Sullivan
-- License: BSD3
--
-- Maintainer: Bryan O'Sullivan <bos@serpentine.com>
-- Stability: unstable
-- Portability: portable

module Data.BloomFilter.Scalable.Mutable
    (
      MSBloom
    , new
    ) where

import qualified Data.BloomFilter.Mutable as MB
import Data.BloomFilter.Mutable.Internal
import Data.BloomFilter.Easy
import Data.BloomFilter.Hash
import Data.STRef
import Control.Monad.ST (ST)
import Data.Word (Word32)

data MSBState s a = MSBState {
      mbs :: [MBloom s a]
    , capacity :: !Int
    , hashFunc :: a -> [Hash]
    , errRates :: [Double]
    }

data MSBloom s a = MSBloom {
      state :: STRef s (MSBState s a)
    , size :: STRef s Int
    }

hasher :: (Hashable a) => a -> [Word32]
hasher v = go 0xb9c53ef3
    where go s = let s' = hashSalt32 s v
                 in s' : go s'

grow :: (a -> [Hash]) -> Int -> Double -> Double -> ST s (MBloom s a)
grow hasher cap errRate tightening = do
  let newCap = cap * 2
      newErrRate = errRate * tightening
      (numBits, numHashes) = case safeSuggestSizing newCap newErrRate of
                               Left _   -> suggestSizing cap errRate
                               Right bh -> bh
  MB.new (take numHashes . hasher) numBits

new :: (a -> [Hash])
    -> Double
    -> Double
    -> ST s (MSBloom s a)
new hasher errRate tightening
    | tightening <= 0 || tightening >= 1 = error "invalid tightening"
    | otherwise = do
  let initCap = 1024
  let (numBits, numHashes) = suggestSizing initCap errRate
  mb <- MB.new (take numHashes . hasher) numBits
  state <- newSTRef MSBState {
             mbs = [mb]
           , capacity = initCap
           , hashFunc = hasher
           , errRates = tail $ iterate (* tightening) errRate
           }
  size <- newSTRef 0
  return $ MSBloom state size
