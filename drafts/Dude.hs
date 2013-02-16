import System.Environment
import qualified DedupeFFI as D
import qualified Haksup.Clustering.KMeans as C
import qualified Data.ByteString as B
import Data.Char
import Data.List
import Control.Monad

instance C.Coordinate B.ByteString where
  distance xs ys = sum . (map (^2)) $ zipWith (-) xs ys
  centroid = liftM2 (divide) (B.foldl1' plus) (fromIntegral . B.length)
    where
      plus = zipWith (+)
      divide xs n = map (/n) (map fromIntegral xs)

main :: IO ()
main = do
  [cs, f] <- getArgs
  let chunkSize = read cs
  (sizeDeduped, sizeOriginal) <- D.dedupeFile chunkSize f
  print (sizeDeduped * chunkSize)
  print (sizeOriginal * chunkSize)
  print (fromIntegral sizeDeduped / fromIntegral sizeOriginal)
