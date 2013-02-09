{-# LANGUAGE ForeignFunctionInterface #-}

import qualified Data.UnorderedMap as UM
import System.Environment
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as Bl

type DummyDedupe = UM.UnorderedMap B.ByteString Int

toBlocks n bs | Bl.null bs = []
              | otherwise = let (block, rest) = Bl.splitAt n bs
                            in block : toBlocks n rest

dedupeBlocks :: DummyDedupe -> [B.ByteString] -> IO ()
dedupeBlocks dd = mapM_ (\block -> insertWith' (+) block 1 $! dd)
  where insertWith' f k nv m = do
          r <- UM.lookup m k
          case r of
            Nothing -> UM.insert m k nv
            Just ov -> UM.insert m k (f nv ov)

dedupeFile :: FilePath -> DummyDedupe -> IO ()
dedupeFile fp dd = Bl.readFile fp >>= dedupeBlocks dd . map toStrict . toBlocks 512
  where toStrict = B.concat . Bl.toChunks
          
main :: IO ()
main = do
  deduper <- UM.empty
  getArgs >>= mapM_ (`dedupeFile` deduper)
  UM.size deduper >>= putStrLn . show . (*512)
  UM.foldM (\acc (_, v) -> return (acc + v)) 0 deduper >>= putStrLn . show . (*512)
