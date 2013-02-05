{-# LANGUAGE ForeignFunctionInterface #-}

module Data.UnorderedMap
       (
         empty
       , insert
       , lookup
       , delete
       , size
       ) where

import Prelude hiding (lookup)
import System.Environment
import Control.Monad
import Foreign hiding (unsafeForeignPtrToPtr)
import Foreign.Ptr
import Foreign.C.Types
import Foreign.ForeignPtr.Unsafe
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as Bi

data STLObject

type UnorderedMap = ForeignPtr STLObject

foreign import ccall unsafe "hashmap.h hashmap_create"
  hashmap_create :: IO (Ptr STLObject)

foreign import ccall unsafe "hashmap.h &hashmap_destroy"
  hashmap_destroy :: FinalizerPtr STLObject

foreign import ccall unsafe "hashmap.h hashmap_insert"
  hashmap_insert :: (Ptr STLObject) -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> IO ()

foreign import ccall unsafe "hashmap.h hashmap_lookup"
  hashmap_lookup :: (Ptr STLObject) -> Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()

foreign import ccall unsafe "hashmap.h hashmap_delete"
  hashmap_delete :: (Ptr STLObject) -> Ptr Word8 -> CSize -> IO ()

foreign import ccall unsafe "hashmap.h hashmap_size"
  hashmap_size :: (Ptr STLObject) -> IO CSize

empty :: IO (UnorderedMap)
empty = hashmap_create >>= newForeignPtr hashmap_destroy

insert :: UnorderedMap -> B.ByteString -> B.ByteString -> IO ()
insert umap0 key0 val0 = do
  let (key1, offK, lenK) = Bi.toForeignPtr key0
      (val1, offV, lenV) = Bi.toForeignPtr val0
      umap = unsafeForeignPtrToPtr umap0
      key = unsafeForeignPtrToPtr key1
      val = unsafeForeignPtrToPtr val1

  hashmap_insert umap (key `plusPtr` offK) (fromIntegral lenK) (val `plusPtr` offV) (fromIntegral lenV)

  touchForeignPtr key1
  touchForeignPtr val1
  touchForeignPtr umap0

insertG :: (Storable a, Storable b) => UnorderedMap -> a -> b -> IO ()
insertG umap0 key val = withForeignPtr umap0 $ \umap -> do
  ptrKey <- new key
  ptrVal <- new val
  hashmap_insert umap (castPtr ptrKey) (fromIntegral . sizeOf $ key)
    (castPtr ptrVal) (fromIntegral . sizeOf $ val)

lookup :: UnorderedMap -> B.ByteString -> IO (Maybe B.ByteString)
lookup umap0 key0 = do
  let (key1, offK, lenK) = Bi.toForeignPtr key0
      umap = unsafeForeignPtrToPtr umap0
      key = unsafeForeignPtrToPtr key1

      lookup_ = with 2 $ \ptrCSize -> do
        with nullPtr $ \ptrPtrCChar -> do
          hashmap_lookup umap (key `plusPtr` offK) (fromIntegral lenK) ptrPtrCChar ptrCSize
          r <- peek ptrCSize
          if r == 0
            then return Nothing
            else do ptrCChar <- peek ptrPtrCChar
                    liftM Just $ Bi.create (fromIntegral r) (\dst -> copyBytes dst ptrCChar (fromIntegral r))
  val <- lookup_

  touchForeignPtr key1
  touchForeignPtr umap0
  return val

delete :: UnorderedMap -> B.ByteString -> IO ()
delete umap0 key0 = do
  let (key1, offK, lenK) = Bi.toForeignPtr key0
      umap = unsafeForeignPtrToPtr umap0
      key = unsafeForeignPtrToPtr key1

  hashmap_delete umap (key `plusPtr` offK) (fromIntegral lenK)

  touchForeignPtr key1
  touchForeignPtr umap0

deleteG :: (Storable a) => UnorderedMap -> a -> IO ()
deleteG umap0 key0 = withForeignPtr umap0 $ \umap -> do
  ptrKey <- new key0
  hashmap_delete umap (castPtr ptrKey) (fromIntegral . sizeOf $ key0)

size :: (Integral a) => UnorderedMap -> IO a
size umap0 = withForeignPtr umap0 $ \umap -> liftM fromIntegral (hashmap_size umap)
