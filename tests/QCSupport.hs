{-# LANGUAGE CPP, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module QCSupport
    (
      P(..)
    ) where

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import qualified Data.ByteString.Char8 as SB
import qualified Data.ByteString.Lazy.Char8 as LB
import System.Random (Random(..), RandomGen)
import Test.QuickCheck

newtype P = P { unP :: Double }
    deriving (Eq, Ord, Show, Fractional, Num, Random)

instance Arbitrary P where
    arbitrary = choose (epsilon, 1 - epsilon)
        where epsilon = 1e-6 :: P

instance Arbitrary Ordering where
    arbitrary = oneof [return LT, return GT, return EQ]

-- For some reason, MIN_VERSION_random doesn't work here :-(
#if __GLASGOW_HASKELL__ < 704
integralRandomR :: (Integral a, RandomGen g) => (a, a) -> g -> (a, g)
integralRandomR (a,b) g = case randomR (fromIntegral a :: Int,
                                        fromIntegral b :: Int) g
                          of (x,g') -> (fromIntegral x, g')

instance Random Int64 where
  randomR = integralRandomR
  random = randomR (minBound,maxBound)
#endif

instance Arbitrary LB.ByteString where
    arbitrary = sized $ \n -> resize (round (sqrt (toEnum n :: Double)))
                ((LB.fromChunks . filter (not . SB.null)) `fmap` arbitrary)

instance Arbitrary SB.ByteString where
    arbitrary = SB.pack `fmap` arbitrary
