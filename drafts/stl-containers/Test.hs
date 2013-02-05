import Prelude hiding (lookup)
import Data.UnorderedMap
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

main = do
  umap <- empty
  insert umap (B8.pack "112233") (B8.pack "aabbcc")
  val <- lookup umap (B8.pack "112233")
  print val
  delete umap (B8.pack "112233")
  size umap >>= print
