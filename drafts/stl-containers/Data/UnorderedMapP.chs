{-# LANGUAGE ForeignFunctionInterface #-}

#include "hashmap.h"

module Data.UnorderedMap
       (
         module Data.Serialize
       , empty
       , insert
       , lookup
       , delete
       , size
       ) where

import c2hs
import Data.Serialize
import Prelude hiding (lookup)
import System.Environment
import Control.Monad
import Foreign hiding (unsafeForeignPtrToPtr)
import Foreign.Ptr
import Foreign.C.Types
import Foreign.ForeignPtr.Unsafe
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as Bi

data UMO  -- unordered_map object
type UnorderedMap_ = ForeignPtr UMO
data UnorderedMap k v = UM {-# UNPACK #-} !UnorderedMap_

foreign import ccall unsafe "hashmap.h hashmap_create"
  hashmap_create :: IO (Ptr UMO)

foreign import ccall unsafe "hashmap.h &hashmap_destroy"
  hashmap_destroy :: FinalizerPtr UMO

foreign import ccall unsafe "hashmap.h hashmap_insert"
  hashmap_insert :: (Ptr UMO) -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> IO ()

foreign import ccall unsafe "hashmap.h hashmap_lookup"
  hashmap_lookup :: (Ptr UMO) -> Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()

foreign import ccall unsafe "hashmap.h hashmap_delete"
  hashmap_delete :: (Ptr UMO) -> Ptr Word8 -> CSize -> IO ()

foreign import ccall unsafe "hashmap.h hashmap_size"
  hashmap_size :: (Ptr UMO) -> IO CSize

data UMIO -- unordered_map::iterator object

foreign import ccall unsafe "hashmap.h iter_create"
  iter_create :: (Ptr UMO) -> IO (Ptr UMIO)

foreign import ccall unsafe "hashmap.h iter_destory"
  iter_destory :: FinalizerPtr UMIO

foreign import ccall unsafe "hashmap.h iter_hasNext"
  iter_hasNext :: (Ptr UMO) -> (Ptr UMIO) -> IO Bool

foreign import ccall unsafe "hashmap.h iter_next"
  iter_next :: (Ptr UMO) -> (Ptr UMIO) -> Ptr (Ptr Word8) -> Ptr CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO Bool

empty :: IO (UnorderedMap k v)
empty = hashmap_create >>= liftM UM . newForeignPtr hashmap_destroy

insertB :: UnorderedMap_ -> B.ByteString -> B.ByteString -> IO ()
insertB umap0 key0 val0 = do
  let (key1, offK, lenK) = Bi.toForeignPtr key0
      (val1, offV, lenV) = Bi.toForeignPtr val0
      umap = unsafeForeignPtrToPtr umap0
      key = unsafeForeignPtrToPtr key1
      val = unsafeForeignPtrToPtr val1

  hashmap_insert umap (key `plusPtr` offK) (fromIntegral lenK) (val `plusPtr` offV) (fromIntegral lenV)

  touchForeignPtr key1
  touchForeignPtr val1
  touchForeignPtr umap0
  
lookupB :: UnorderedMap_ -> B.ByteString -> IO (Maybe B.ByteString)
lookupB umap0 key0 = do
  let (key1, offK, lenK) = Bi.toForeignPtr key0
      umap = unsafeForeignPtrToPtr umap0
      key = unsafeForeignPtrToPtr key1
      lookup_ = with 2 $ \pNV ->
        with nullPtr $ \ppVal -> do
          hashmap_lookup umap (key `plusPtr` offK) (fromIntegral lenK) ppVal pNV
          nV <- peek pNV
          if nV == 0
            then return Nothing
            else do pVal <- peek ppVal
                    liftM Just $ Bi.create (fromIntegral nV) (\dst -> copyBytes dst pVal (fromIntegral nV))
  val <- lookup_

  touchForeignPtr key1
  touchForeignPtr umap0
  return val

-- foldMB :: (a -> (ByteString, ByteString) -> IO a) -> a -> UnorderedMap_ -> IO a
-- foldMB f acc0 umap0 = withForeignPtr umap0 $ \umap -> do
--   it0 <- iter_create umap
--   result <- loop acc0 it0
--   iter_destory it0
--   return result
--   where loop :: a -> (Ptr UMIO) -> IO a
--         loop acc it = do
--           hasNext <- iter_hasNext umap it
--           if not hasNext
--             then return acc
--             else do iter_next umap it
            
--                  loop (f acc (key, val)) 
--           loop acc' 
  

deleteB :: UnorderedMap_ -> B.ByteString -> IO ()
deleteB umap0 key0 = do
  let (key1, offK, lenK) = Bi.toForeignPtr key0
      umap = unsafeForeignPtrToPtr umap0
      key = unsafeForeignPtrToPtr key1

  hashmap_delete umap (key `plusPtr` offK) (fromIntegral lenK)

  touchForeignPtr key1
  touchForeignPtr umap0

insert :: (Serialize k, Serialize v) => UnorderedMap k v -> k -> v -> IO ()
insert (UM umap) key val = insertB umap (encode key) (encode val)

lookup :: (Serialize k, Serialize v) => UnorderedMap k v -> k -> IO (Maybe v)
lookup (UM umap) key = do
  res <- lookupB umap (encode key)
  case res of
    Nothing -> return Nothing
    Just val -> either (const (return Nothing)) (return . Just) (decode val)

delete :: (Serialize k) => UnorderedMap k v -> k -> IO ()
delete (UM umap) key = deleteB umap (encode key)

size :: (Integral a) => UnorderedMap k v -> IO a
size (UM umap0) = withForeignPtr umap0 $ \umap -> liftM fromIntegral (hashmap_size umap)

-- foldM :: (Serialize k, Serialize v) => (a -> (k, v) -> IO a) -> a -> UnorderedMap k v -> IO a
-- FoldM f acc (UM umap) = do
  


insertS :: (Storable k, Storable v) => UnorderedMap k v -> k -> v -> IO ()
insertS (UM umap0) key val = withForeignPtr umap0 $ \umap -> do
  ptrKey <- new key
  ptrVal <- new val
  hashmap_insert umap (castPtr ptrKey) (fromIntegral . sizeOf $ key)
    (castPtr ptrVal) (fromIntegral . sizeOf $ val)

deleteS :: (Storable k) => UnorderedMap k v -> k -> IO ()
deleteS (UM umap0) key0 = withForeignPtr umap0 $ \umap -> do
  ptrKey <- new key0
  hashmap_delete umap (castPtr ptrKey) (fromIntegral . sizeOf $ key0)
