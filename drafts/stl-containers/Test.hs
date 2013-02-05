{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MagicHash #-}

import GHC.Prim
import GHC.Generics
import Prelude hiding (lookup)
import Data.UnorderedMap
import Data.Word
import Data.ByteString.Char8 hiding (empty)

data Value = V Word8 Word8 Word8 deriving (Generic, Show)

instance Serialize Value

main = do
  umap <- empty
  insert umap (pack "") (V 1 2 3)
  Just (val :: Value) <- lookup umap (pack "")
  print val
  delete umap (pack "")
  size umap >>= print
