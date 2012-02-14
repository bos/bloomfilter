{-# LANGUAGE CPP, FlexibleContexts, ForeignFunctionInterface, MagicHash,
             Rank2Types, UnliftedFFITypes #-}

module Data.BloomFilter.Array (newArray) where

import Control.Monad.ST (ST, unsafeIOToST)
import Data.Array.Base (MArray, STUArray(..), unsafeNewArray_)
#if __GLASGOW_HASKELL__ >= 704
import Foreign.C.Types (CInt(..), CSize(..))
#else
import Foreign.C.Types (CInt, CSize)
#endif
import Foreign.Ptr (Ptr)
import GHC.Base (MutableByteArray#)

newArray :: forall e s. (MArray (STUArray s) e (ST s)) =>
            Int -> Int -> ST s (STUArray s Int e)
{-# INLINE newArray #-}
newArray numElems numBytes = do
  ary@(STUArray _ _ _ marr#) <- unsafeNewArray_ (0, numElems - 1)
  _ <- unsafeIOToST (memset marr# 0 (fromIntegral numBytes))
  return ary

foreign import ccall unsafe "memset"
    memset :: MutableByteArray# s -> CInt -> CSize -> IO (Ptr a)
