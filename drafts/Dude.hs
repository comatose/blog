import System.Environment
import qualified DedupeFFI as D
import qualified Haksup.Clustering.KMeans as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as Bl
import Data.Char
import Data.List
import Control.Monad
import Data.Serialize

main :: IO ()
main = do
  [cs, f] <- getArgs
  let chunkSize = read cs
  -- D.kmeansDedupe chunkSize f
  (sizeDeduped, sizeOriginal) <- D.dedupeFile chunkSize f
  print (sizeDeduped * chunkSize)
  print (sizeOriginal * chunkSize)
  print (fromIntegral sizeDeduped / fromIntegral sizeOriginal)
