module Main where

import Control.Monad (forM_)
import Data.BloomFilter.Easy (easyList, elemB)
import Data.BloomFilter.Hash (Hashable(..), hash64)
import qualified Data.ByteString.Char8 as SB
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import System.IO (BufferMode(..), hSetBuffering, stdout)
import Test.Framework (Test, defaultMain)
import Test.QuickCheck (Property, Testable, (==>), choose, forAll)

import QCSupport (P(..))

prop_pai :: (Hashable a) => a -> a -> P -> Bool
prop_pai _ xs (P q) = let bf = easyList q [xs] in xs `elemB` bf

tests :: [Test]
tests = [
   testProperty "()" $ prop_pai ()
 , testProperty "Bool" $ prop_pai (undefined :: Bool)
 , testProperty "Ordering" $ prop_pai (undefined :: Ordering)
 , testProperty "Char" $ prop_pai (undefined :: Char)
 , testProperty "Int" $ prop_pai (undefined :: Int)
 , testProperty "Float" $ prop_pai (undefined :: Float)
 , testProperty "Double" $ prop_pai (undefined :: Double)
 , testProperty "Int8" $ prop_pai (undefined :: Int8)
 , testProperty "Int16" $ prop_pai (undefined :: Int16)
 , testProperty "Int32" $ prop_pai (undefined :: Int32)
 , testProperty "Int64" $ prop_pai (undefined :: Int64)
 , testProperty "Word8" $ prop_pai (undefined :: Word8)
 , testProperty "Word16" $ prop_pai (undefined :: Word16)
 , testProperty "Word32" $ prop_pai (undefined :: Word32)
 , testProperty "Word64" $ prop_pai (undefined :: Word64)
 , testProperty "String" $ prop_pai (undefined :: String)
 , testProperty "LB.ByteString" $ prop_pai (undefined :: LB.ByteString)
 , testProperty "prop_rechunked_eq" prop_rechunked_eq
 ]

rechunk :: Int64 -> LB.ByteString -> LB.ByteString
rechunk k xs | k <= 0    = xs
             | otherwise = LB.fromChunks (go xs)
    where go s | LB.null s = []
               | otherwise = let (pre,suf) = LB.splitAt k s
                             in  repack pre : go suf
          repack = SB.concat . LB.toChunks

-- Ensure that a property over a lazy ByteString holds if we change
-- the chunk boundaries.
prop_rechunked :: Eq a => (LB.ByteString -> a) -> LB.ByteString -> Property
prop_rechunked f s =
    let l = LB.length s
    in l > 0 ==> forAll (choose (1,l-1)) $ \k ->
        let n = k `mod` l
        in n > 0 ==> f s == f (rechunk n s)

prop_rechunked_eq :: LB.ByteString -> Property
prop_rechunked_eq = prop_rechunked hash64

main :: IO ()
main = defaultMain tests
