module Main where

import Control.Monad (forM_)
import Data.BloomFilter.Easy (easyList, elemB)
import Data.BloomFilter.Hash (Hashable(..))
import qualified Data.ByteString.Char8 as SB
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import System.Environment (getArgs)
import System.IO (BufferMode(..), hSetBuffering, stdout)
import Test.QuickCheck (Testable)

import QCSupport

prop_pai :: (Hashable a) => a -> (a, P) -> Bool
prop_pai _ (xs, P q) = let bf = easyList q [xs] in xs `elemB` bf

p :: Testable a => String -> a -> (Int -> IO (), String)
p desc prop = (\count -> limCheck count prop, desc)

tests :: [(Int -> IO (), String)]
tests = [
   p "()" $ prop_pai ()
 , p "Bool" $ prop_pai (undefined :: Bool)
 , p "Ordering" $ prop_pai (undefined :: Ordering)
 , p "Char" $ prop_pai (undefined :: Char)
 , p "Int" $ prop_pai (undefined :: Int)
 , p "Float" $ prop_pai (undefined :: Float)
 , p "Double" $ prop_pai (undefined :: Double)
 , p "Int8" $ prop_pai (undefined :: Int8)
 , p "Int16" $ prop_pai (undefined :: Int16)
 , p "Int32" $ prop_pai (undefined :: Int32)
 , p "Int64" $ prop_pai (undefined :: Int64)
 , p "Word8" $ prop_pai (undefined :: Word8)
 , p "Word16" $ prop_pai (undefined :: Word16)
 , p "Word32" $ prop_pai (undefined :: Word32)
 , p "Word64" $ prop_pai (undefined :: Word64)
 , p "String" $ prop_pai (undefined :: String)
 , p "LB.ByteString" $ prop_pai (undefined :: LB.ByteString)
 , p "SB.ByteString" $ prop_pai (undefined :: SB.ByteString)
 , p "(String,Int)" $ prop_pai (undefined :: (String,Int))
 , p "(Double,Int,SB.ByteString)" $ prop_pai
         (undefined :: (Double,Int,SB.ByteString))
 , p "(Ordering,(Char,Int),LB.ByteString,Word64)" $ prop_pai
         (undefined :: (Ordering,(Char,Int),LB.ByteString,Word64))
 ]

main :: IO ()
main = do
  args <- getArgs
  let limits = case args of
                [] -> [100]
                xs -> map read xs
  hSetBuffering stdout NoBuffering
  forM_ limits $ \limit ->
    forM_ tests $ \(test, desc) -> do
      putStr $ desc ++ ": "
      test limit
