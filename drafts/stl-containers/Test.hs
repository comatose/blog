{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Data.ByteString.Char8 hiding (empty)
import           Data.UnorderedMap
import           Data.Word
import           GHC.Generics
import           GHC.Prim
import           Prelude               hiding (length, lookup)

data Value = V Word8 Word8 Word8 deriving (Generic, Show)

instance Serialize Value

main = do
  umap <- empty
  insertS umap (pack "2") (pack "22")
  -- insertS umap 1 3
  lookupS umap (pack "2") >>= print
  size umap >>= print
  -- val <- foldMS addM 0 umap
  -- print val
  -- deleteS umap (pack "")


  where addM acc (x, y) = return $ acc + length y
