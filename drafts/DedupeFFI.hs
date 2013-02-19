{-# LANGUAGE BangPatterns #-}

module DedupeFFI where

import Data.UnorderedMap
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as Bl
import Prelude hiding(lookup)

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



  
