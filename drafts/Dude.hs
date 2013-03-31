import System.Environment
import qualified DedupeFFI as D (countUnique, toStrict, toBlocks, dedupeFile)
import qualified Haksup.Clustering.KMeans as C
import qualified Data.ByteString.Lazy as Bl
import Data.List (foldl1')
import Data.Int (Int32)
import Data.Serialize (decodeLazy)
import Control.Monad (zipWithM_)
import qualified DummyDedupe as DD (countUnique)

kmeansDedupe :: Int -> FilePath -> IO (Int, Int)
kmeansDedupe block_size fp = do
  src <- Bl.readFile fp
  let cls = C.kmeansGen conv 4 . D.toBlocks (block_size * 128) $ src
  res <- mapM (D.countUnique . map D.toStrict . concatMap (D.toBlocks block_size)) cls
  print res
  return . foldl1' (\(a1, b1) (a2, b2) -> (a1 + a2, b1 + b2)) $ res
  
  where conv :: Bl.ByteString -> C.Point
        conv = map (fromIntegral . toInt32) . D.toBlocks 4
        toInt32 :: Bl.ByteString -> Int32
        toInt32 = either (const 0) id . decodeLazy

dummyDedupe :: Int -> FilePath -> IO (Int, Int)
dummyDedupe block_size fp = do
  src <- Bl.readFile fp
  return . DD.countUnique . D.toBlocks (block_size) $ src

main :: IO ()
main = do
  [op, cs, f] <- getArgs
  let chunkSize = read cs
<<<<<<< HEAD
  -- D.kmeansDedupe chunkSize f
  (sizeDeduped, sizeOriginal) <- D.dedupeFile chunkSize f
=======
  (sizeDeduped, sizeOriginal) <- case op of
    "dd" -> D.dedupeFile chunkSize f
    "cd" -> kmeansDedupe chunkSize f
    "hd" -> dummyDedupe chunkSize f
>>>>>>> 51fb2daa2af23bc7656d3e6befdd09dd69e0e1e7
  print (sizeDeduped * chunkSize)
  print (sizeOriginal * chunkSize)
  print (fromIntegral sizeDeduped / fromIntegral sizeOriginal)
