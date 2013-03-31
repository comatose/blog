{-# LANGUAGE BangPatterns #-}

module DedupeFFI where

import Data.Serialize (decodeLazy)
import Data.Int (Int32)
import Data.UnorderedMap
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as Bl
import Prelude hiding(lookup)
import Haksup.Clustering.KMeans

-- type DummyDedupe = UnorderedMap B.ByteString Int

toBlocks :: Int -> Bl.ByteString -> [Bl.ByteString]
toBlocks n bstr | Bl.null bstr = []
                | otherwise = let (block, rest) = Bl.splitAt (fromIntegral n) bstr
                              in block : toBlocks n rest

toStrict :: Bl.ByteString -> B.ByteString
toStrict = B.concat . Bl.toChunks

countUnique :: [B.ByteString] -> IO (Int, Int)
countUnique bs = do
  dd <- empty
  mapM_ (\block -> insertWith' (+) block 1 dd) bs
  cntUniq <- size dd
  cntAll <- foldM' (\acc (_, v) -> return (acc + v)) (0 :: Int) dd
  return (cntUniq, cntAll)
  
  where
    insertWith' f k nv m = lookup m k >>= maybe (insert m k nv) ((insert m k $!). f nv)

dedupeFile :: Int -> FilePath -> IO (Int, Int)
dedupeFile block_size fp = Bl.readFile fp >>= countUnique . map toStrict . toBlocks block_size

kmeansDedupe block_size fp = Bl.readFile fp >>= print . kmeansGen conv 4 . toBlocks (block_size * 16)
  where conv :: Bl.ByteString -> Point
        conv = map (fromIntegral . f) . toBlocks 4
        f :: Bl.ByteString -> Int32
        f = either (const 0) id . decodeLazy
        
