{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Data.Storable.UnorderedMap
       (
       UnorderedMap

       , empty
       , insert
       , lookup
       , delete
       , size
       , foldM
       , foldM'
       ) where

import Data.IORef
import Data.Serialize
import Prelude hiding (lookup)
import Control.Monad hiding (foldM)
import Foreign hiding (unsafeForeignPtrToPtr)
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

foreign import ccall unsafe "hashmap.h iter_destroy"
  iter_destroy :: (Ptr UMIO) -> IO ()

foreign import ccall unsafe "hashmap.h iter_hasNext"
  iter_hasNext :: (Ptr UMO) -> (Ptr UMIO) -> IO Bool

foreign import ccall unsafe "hashmap.h iter_next"
  iter_next :: (Ptr UMO) -> (Ptr UMIO) -> Ptr (Ptr Word8) -> Ptr CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()

instance Storable B.ByteString where
  -- sizeOf (Bi.PS _ _ l) = sizeOf l + l
  sizeOf _ = 100
  alignment _ = alignment (undefined :: Ptr Word8)
  poke dst (Bi.PS fp o l) =
    withForeignPtr fp $ \p -> do
      poke (castPtr dst) l
      copyBytes (dst `plusPtr` sizeOf l) (p `plusPtr` o) l
  peek src = do
    len <- peek (castPtr src) :: IO Int
    Bi.create len (\dst -> copyBytes dst (castPtr src `plusPtr` sizeOf len) len)

empty :: IO (UnorderedMap k v)
empty = hashmap_create >>= liftM UM . newForeignPtr hashmap_destroy

size :: (Integral a) => UnorderedMap k v -> IO a
size (UM umap0) = withForeignPtr umap0 $ \umap -> liftM fromIntegral (hashmap_size umap)

insert :: (Storable k, Storable v) => UnorderedMap k v -> k -> v -> IO ()
insert (UM umap0) key val =
  withForeignPtr umap0 $ \umap ->
  with key $ \pKey ->
  with val $ \pVal ->
  hashmap_insert umap (castPtr pKey) (fromIntegral . sizeOf $ key) (castPtr pVal) (fromIntegral . sizeOf $ val)
  
lookup :: (Storable k, Storable v) => UnorderedMap k v -> k -> IO (Maybe v)
lookup (UM umap0) key =
  withForeignPtr umap0 $ \umap ->
  with key $ \pKey ->
  with 2 $ \pNV ->
  with nullPtr $ \ppVal -> do
    hashmap_lookup umap (castPtr pKey) (fromIntegral . sizeOf $ key) ppVal pNV
    nV <- peek pNV
    if nV == 0
      then return Nothing
      else liftM Just $ peek (castPtr ppVal)

delete :: (Storable k) => UnorderedMap k v -> k -> IO ()
delete (UM umap0) key = 
  withForeignPtr umap0 $ \umap ->
  with key $ \pKey -> do
    hashmap_delete umap (castPtr pKey) (fromIntegral . sizeOf $ key)

foldM :: (Storable k, Storable v) => (a -> (k, v) -> IO a) -> a -> UnorderedMap k v -> IO a
foldM f acc0 (UM umap0) = withForeignPtr umap0 $ \umap -> do
  acc <- newIORef acc0
  it <- iter_create umap
  loop umap it acc
  iter_destroy it
  readIORef acc
    where 
      -- loop :: Ptr UMO -> Ptr UMIO -> IORef a -> IO ()
      loop umap it acc = do
        hasNext <- iter_hasNext umap it
        if not hasNext
          then return ()
          else do
          with 2 $ \pNK -> with 2 $ \pNV ->
            with nullPtr $ \ppKey -> with nullPtr $ \ppVal -> do
              iter_next umap it ppKey pNK ppVal pNV
              -- nK <- peek pNK
              -- nV <- peek pNV
              key <- peek ppKey >>= peek . castPtr
              val <- peek ppVal >>= peek . castPtr
              readIORef acc >>= (`f` (key, val)) >>= (writeIORef acc $!)
          loop umap it acc


foldM' :: (Storable k, Storable v) => (a -> (k, v) -> IO a) -> a -> UnorderedMap k v -> IO a
foldM' f acc umap = foldM f' acc umap
  where f' !a (k, v) = f a (k, v)
