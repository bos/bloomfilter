{-# LANGUAGE FlexibleContexts, ForeignFunctionInterface, MagicHash,
             Rank2Types #-}
{-# OPTIONS_GHC -fglasgow-exts #-}

module Data.BloomFilter.Array (newArray) where

import Control.Monad.ST (ST, unsafeIOToST)
import Data.Array.Base (MArray, STUArray(..), unsafeNewArray_)
import Foreign.C.Types (CInt, CSize)
import Foreign.Ptr (Ptr)
import GHC.Base (MutableByteArray#)

newArray :: forall e s. (MArray (STUArray s) e (ST s)) =>
            Int -> Int -> ST s (STUArray s Int e)
{-# INLINE newArray #-}
newArray numElems numBytes = do
  ary@(STUArray _ _ _ marr#) <- unsafeNewArray_ (0, numElems - 1)
  unsafeIOToST (memset marr# 0 (fromIntegral numBytes))
  return ary

foreign import ccall unsafe "memset"
    memset :: MutableByteArray# s -> CInt -> CSize -> IO (Ptr a)
