{-# LANGUAGE OverloadedStrings #-}
import Data.ByteString.Lazy.Internal (ByteString(..))
import Data.Char
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.BloomFilter.Hash
import Data.String

instance IsString L.ByteString where
    fromString = L.pack

instance IsString S.ByteString where
    fromString = S.pack

reindex s = (L.mapIndexed go s, map S.length (L.toChunks s))
    where go k _ = case chr (fromIntegral k `mod` 10 + ord 'a') of
                     'j' -> ' '
                     c -> c

main = do
  let a = Chunk "abcdefghi ab" (Chunk "cdefghi abcd" Empty)
      b = Chunk "abcdef" (Chunk "gh" (Chunk "i " (Chunk "abcdefgh" (Chunk "i a" (Chunk "bcd" Empty)))))
  print (reindex a)
  print (reindex b)
  print (L.length a, a == b)
  let (ha, hb) = (hash64 a, hash64 b)
  print (ha, hb)
  print $ ha == hb
