{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Data.UnorderedMap
       (
       UnorderedMap

       , empty
       , emptySized
       , insert
       , lookup
       , delete
       , size
       , foldM
       , foldM'
       , toList
       , fromList
       ) where

import Data.IORef
import Data.Serialize
import Prelude hiding (lookup)
import Control.Monad hiding (foldM)
import Foreign hiding (unsafeForeignPtrToPtr)
import Foreign.C.Types
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as Bi

data UMO  -- unordered_map object
type UnorderedMap_ = ForeignPtr UMO
data UnorderedMap k v = UM {-# UNPACK #-} !UnorderedMap_

foreign import ccall unsafe "hashmap.h hashmap_create"
  hashmap_create :: IO (Ptr UMO)

foreign import ccall unsafe "hashmap.h hashmap_create_sized"
  hashmap_create_sized :: CSize -> IO (Ptr UMO)

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

empty :: IO (UnorderedMap k v)
empty = hashmap_create >>= liftM UM . newForeignPtr hashmap_destroy

emptySized :: Int -> IO (UnorderedMap k v)
emptySized s = hashmap_create_sized (fromIntegral s) >>= liftM UM . newForeignPtr hashmap_destroy

insertB :: UnorderedMap_ -> B.ByteString -> B.ByteString -> IO ()
insertB umap0 (Bi.PS key0 offK lenK) (Bi.PS val0 offV lenV) =
  withForeignPtr umap0 $ \umap ->
  withForeignPtr key0 $ \key ->
  withForeignPtr val0 $ \val ->
  hashmap_insert umap (key `plusPtr` offK) (fromIntegral lenK) (val `plusPtr` offV) (fromIntegral lenV)
  
lookupB :: UnorderedMap_ -> B.ByteString -> IO (Maybe B.ByteString)
lookupB umap0 (Bi.PS key0 offK lenK) =
  withForeignPtr umap0 $ \umap ->
  withForeignPtr key0 $ \key ->
  with 0 $ \pNV ->
  with nullPtr $ \ppVal -> do
    hashmap_lookup umap (key `plusPtr` offK) (fromIntegral lenK) ppVal pNV
    nV <- peek pNV
    if nV == 0
      then return Nothing
      else do pVal <- peek ppVal
              liftM Just $ Bi.create (fromIntegral nV) (\dst -> copyBytes dst pVal (fromIntegral nV))

deleteB :: UnorderedMap_ -> B.ByteString -> IO ()
deleteB umap0 (Bi.PS key0 offK lenK) =
  withForeignPtr umap0 $ \umap ->
  withForeignPtr key0 $ \key ->
  hashmap_delete umap (key `plusPtr` offK) (fromIntegral lenK)

foldMB :: (a -> (B.ByteString, B.ByteString) -> IO a) -> a -> UnorderedMap_ -> IO a
foldMB f acc0 umap0 = withForeignPtr umap0 $ \umap -> do
  acc <- newIORef acc0
  it <- iter_create umap
  loop umap it acc
  iter_destroy it
  readIORef acc
    where 
      loop umap it acc = do
        hasNext <- iter_hasNext umap it
        if not hasNext
          then return ()
          else do
          with 2 $ \pNK -> with 2 $ \pNV ->
            with nullPtr $ \ppKey -> with nullPtr $ \ppVal -> do
              iter_next umap it ppKey pNK ppVal pNV
              nK <- peek pNK
              nV <- peek pNV
              pKey <- peek ppKey
              pVal <- peek ppVal
              key <- Bi.create (fromIntegral nK) (\dst -> copyBytes dst pKey (fromIntegral nK))
              val <- Bi.create (fromIntegral nV) (\dst -> copyBytes dst pVal (fromIntegral nV))
              readIORef acc >>= (`f` (key, val)) >>= (writeIORef acc)
          loop umap it acc

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

foldM :: (Serialize k, Serialize v) => (a -> (k, v) -> IO a) -> a -> UnorderedMap k v -> IO a
foldM f acc (UM umap) = foldMB f' acc umap
  where f' a (k, v) = f a (right . decode $ k, right . decode $ v)
        right (Right x) = x
        right _ = error "cereal decode failed."

foldM' :: (Serialize k, Serialize v) => (a -> (k, v) -> IO a) -> a -> UnorderedMap k v -> IO a
foldM' f = foldM (\ !a -> f a)

toList :: (Serialize k, Serialize v) => UnorderedMap k v -> IO [(k, v)]
toList = foldM f []
  where f list entry = return (entry:list)

fromList :: (Serialize k, Serialize v) => [(k, v)] -> IO (UnorderedMap k v)
fromList list = do
  um <- empty
  mapM_ (uncurry (insert um)) list
  return um
