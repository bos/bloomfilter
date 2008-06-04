{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module QCSupport
    (
      P(..)
    , limCheck
    ) where

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import qualified Data.ByteString.Char8 as SB
import qualified Data.ByteString.Lazy.Char8 as LB
import System.Random (Random(..), RandomGen)
import Test.QuickCheck

limCheck :: Testable a => Int -> a -> IO ()
limCheck limit = check defaultConfig {
                   configMaxTest = limit
                 , configEvery = \_ _ -> ""
                 }

newtype P = P { unP :: Double }
    deriving (Eq, Ord, Show, Fractional, Num, Random)

instance Arbitrary P where
    arbitrary = choose (epsilon, 1 - epsilon)
        where epsilon = 1e-4 :: P
    coarbitrary = coarbitrary . decodeFloat . unP

instance Arbitrary Ordering where
    arbitrary = oneof [return LT, return GT, return EQ]
    coarbitrary = coarbitrary . fromEnum

instance Arbitrary Char where
    arbitrary = choose (minBound, maxBound)
    coarbitrary = coarbitrary . fromEnum

integralRandomR :: (Integral a, RandomGen g) => (a, a) -> g -> (a, g)
integralRandomR (a,b) g = case randomR (fromIntegral a :: Int,
                                        fromIntegral b :: Int) g
                          of (x,g') -> (fromIntegral x, g')

instance Random Int8 where
  randomR = integralRandomR
  random = randomR (minBound,maxBound)

instance Random Int16 where
  randomR = integralRandomR
  random = randomR (minBound,maxBound)

instance Random Int32 where
  randomR = integralRandomR
  random = randomR (minBound,maxBound)

instance Random Int64 where
  randomR = integralRandomR
  random = randomR (minBound,maxBound)

instance Random Word8 where
  randomR = integralRandomR
  random = randomR (minBound,maxBound)

instance Random Word16 where
  randomR = integralRandomR
  random = randomR (minBound,maxBound)

instance Random Word32 where
  randomR = integralRandomR
  random = randomR (minBound,maxBound)

instance Random Word64 where
  randomR = integralRandomR
  random = randomR (minBound,maxBound)

integralCoarbitrary :: (Integral a) => a -> Gen b -> Gen b
integralCoarbitrary n = variant (if n' >= 0 then 2*n' else 2*(-n') + 1)
        where n' = fromIntegral n

instance Arbitrary Int8 where
    arbitrary = choose (minBound, maxBound)
    coarbitrary = integralCoarbitrary

instance Arbitrary Int16 where
    arbitrary = choose (minBound, maxBound)
    coarbitrary = integralCoarbitrary

instance Arbitrary Int32 where
    arbitrary = choose (minBound, maxBound)
    coarbitrary = integralCoarbitrary

instance Arbitrary Int64 where
    arbitrary = choose (minBound, maxBound)
    coarbitrary = integralCoarbitrary

instance Arbitrary Word8 where
    arbitrary = choose (minBound, maxBound)
    coarbitrary = integralCoarbitrary

instance Arbitrary Word16 where
    arbitrary = choose (minBound, maxBound)
    coarbitrary = integralCoarbitrary

instance Arbitrary Word32 where
    arbitrary = choose (minBound, maxBound)
    coarbitrary = integralCoarbitrary

instance Arbitrary Word64 where
    arbitrary = choose (minBound, maxBound)
    coarbitrary = integralCoarbitrary

instance Arbitrary LB.ByteString where
    arbitrary = LB.pack `fmap` arbitrary
    coarbitrary = coarbitrary . LB.unpack

instance Arbitrary SB.ByteString where
    arbitrary = SB.pack `fmap` arbitrary
    coarbitrary = coarbitrary . SB.unpack
