{-# LANGUAGE GeneralizedNewtypeDeriving, NoMonomorphismRestriction #-}

module Data.BloomFilter.QuickCheck where

import Data.BloomFilter
import Data.BloomFilter.Hash
import Data.BloomFilter.Util
import Data.Word (Word8)
import qualified Data.ByteString.Char8 as SB
import qualified Data.ByteString.Lazy.Char8 as LB
import Foreign.Storable (Storable)
import System.Random (Random(..))
import Test.QuickCheck

limCheck :: Testable a => Int -> a -> IO ()
limCheck limit = check defaultConfig {
                   configMaxTest = limit
                 , configEvery = \_ _ -> ""
                 }

newtype P = P { unP :: Double }
    deriving (Eq, Ord, Show, Fractional, Num, Random)

instance Arbitrary P where
    arbitrary = chooseP
    coarbitrary = coarbitrary . decodeFloat . unP

instance Arbitrary Char where
    arbitrary = choose (minBound, maxBound)
    coarbitrary = coarbitrary . fromEnum

instance Random Word8 where
  randomR (a,b) g = case randomR (fromIntegral a :: Int,
                                  fromIntegral b :: Int) g
                    of (x,g') -> (fromIntegral x, g')
  random = randomR (minBound,maxBound)

instance Arbitrary Word8 where
    arbitrary = choose (minBound, maxBound)
    coarbitrary n = variant (if n' >= 0 then 2*n' else 2*(-n') + 1)
        where n' = fromIntegral n

instance Arbitrary SB.ByteString where
    arbitrary = SB.pack `fmap` arbitrary
    coarbitrary = coarbitrary . SB.unpack

chooseP = choose (epsilon, 1 - epsilon)
    where epsilon = 1e-4 :: P

prop_presentAfterInsert :: (Hashable a) => a -> (a, P) -> Bool
prop_presentAfterInsert _ (xs, P p) = let bf = easyList p [xs]
                                      in xs `elemB` bf

prop_pai_list :: (Hashable a) => a -> ([a], P) -> Bool
prop_pai_list _ (xs, P p) = let bf = easyList p xs
                            in all (`elemB` bf) xs

prop_pai_Int = prop_presentAfterInsert (undefined :: Int)
prop_pai_String = prop_presentAfterInsert (undefined :: String)
prop_pai_BS = prop_presentAfterInsert (undefined :: SB.ByteString)
