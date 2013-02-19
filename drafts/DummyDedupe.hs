{-# LANGUAGE BangPatterns #-}

module DummyDedupe
       (
         module Data.HashMap.Strict
       , dedupeBlocks
       , dedupePath
       , countUnique
       )
       where

import           Control.Monad
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as Bl
import Data.HashMap.Strict
import           Data.Int
import qualified Data.List            as DL
import           System.Directory
import Control.Arrow ((&&&))
import Data.Hashable
import qualified Data.Foldable as DF

type DummyDedupe = HashMap B.ByteString Int64

toBlocks :: Int64 -> Bl.ByteString -> [Bl.ByteString]
toBlocks n bs | Bl.null bs = []
              | otherwise = let (block, rest) = Bl.splitAt n bs
                            in block : toBlocks n rest

dedupeBlocks :: [B.ByteString] -> DummyDedupe -> DummyDedupe
dedupeBlocks = flip $ DL.foldl' (\ !acc block -> insertWith (+) block 1 acc)

countUnique :: (Hashable k, Ord k) => [k] -> (Int, Int)
countUnique = (size &&& DF.sum) . DL.foldl' (\acc k -> insertWith (+) k 1 acc) empty

dedupePath :: FilePath -> DummyDedupe -> IO DummyDedupe
dedupePath fp dd = do
  isFile <- doesFileExist fp
  if isFile
    then putStrLn fp >> Bl.readFile fp >>= return . (`dedupeBlocks` dd) . DL.map toStrict . toBlocks 512
    else do isFolder <- doesDirectoryExist fp
            if isFolder
              then getDirectoryContents fp >>= foldM (flip dedupePath) dd . (drop 2)
              else return dd

toStrict = B.concat . Bl.toChunks

dedupeFile :: Int -> FilePath -> IO (Int, Int)
dedupeFile chunkSize file = do
  src <- Bl.readFile file
  let res = (`dedupeBlocks` empty) . DL.map toStrict . toBlocks (fromIntegral chunkSize) $ src
  return (size res, fromIntegral $ foldl' (+) 0 res)

