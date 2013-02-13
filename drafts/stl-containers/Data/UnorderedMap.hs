{-# LANGUAGE ForeignFunctionInterface #-}

module Data.UnorderedMap
       (
         module Data.Serialize
       ,UnorderedMap

       , empty
       , insert
       , lookup
       , delete
       , size
       , foldM

       , insertS
       , lookupS
       , deleteS
       , foldMS

       , emptyB
       , insertB
       , lookupB
       , deleteB
       , foldMB
       ) where

import Data.IORef
import Data.Serialize
import Prelude hiding (lookup)
import System.Environment
import Control.Monad hiding (foldM)
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

foreign import ccall unsafe "hashmap.h iter_destroy"
  iter_destroy :: (Ptr UMIO) -> IO ()

foreign import ccall unsafe "hashmap.h iter_hasNext"
  iter_hasNext :: (Ptr UMO) -> (Ptr UMIO) -> IO Bool

foreign import ccall unsafe "hashmap.h iter_next"
  iter_next :: (Ptr UMO) -> (Ptr UMIO) -> Ptr (Ptr Word8) -> Ptr CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO Bool

empty :: IO (UnorderedMap k v)
empty = hashmap_create >>= liftM UM . newForeignPtr hashmap_destroy

emptyB :: IO (UnorderedMap_)
emptyB = hashmap_create >>= newForeignPtr hashmap_destroy

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
              then readIORef acc
              else
              with 2 $ \pNK ->
              with 2 $ \pNV ->
              with nullPtr $ \ppKey ->
              with nullPtr $ \ppVal -> do
                iter_next umap it ppKey pNK ppVal pNV
                nK <- peek pNK
                nV <- peek pNV
                pKey <- peek ppKey
                pVal <- peek ppVal
                key <- Bi.create (fromIntegral nK) (\dst -> copyBytes dst pKey (fromIntegral nK))
                val <- Bi.create (fromIntegral nV) (\dst -> copyBytes dst pVal (fromIntegral nV))
                readIORef acc >>= (`f` (key, val)) >>= writeIORef acc
                loop umap it acc

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

foldM :: (Serialize k, Serialize v) => (a -> (k, v) -> IO a) -> a -> UnorderedMap k v -> IO a
foldM f acc (UM umap) = foldMB f' acc umap
  where f' a (k, v) = f a (right . decode $ k, right . decode $ v)
        right (Right x) = x

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

insertS :: (Storable k, Storable v) => UnorderedMap k v -> k -> v -> IO ()
insertS (UM umap0) key val =
  withForeignPtr umap0 $ \umap ->
  with key $ \pKey ->
  with val $ \pVal ->
  hashmap_insert umap (castPtr pKey) (fromIntegral . sizeOf $ key) (castPtr pVal) (fromIntegral . sizeOf $ val)
  
lookupS :: (Storable k, Storable v) => UnorderedMap k v -> k -> IO (Maybe v)
lookupS (UM umap0) key =
  withForeignPtr umap0 $ \umap ->
  with key $ \pKey ->
  with 2 $ \pNV ->
  with nullPtr $ \ppVal -> do
    hashmap_lookup umap (castPtr pKey) (fromIntegral . sizeOf $ key) ppVal pNV
    nV <- peek pNV
    if nV == 0
      then return Nothing
      else liftM Just $ peek (castPtr ppVal)

deleteS :: (Storable k) => UnorderedMap k v -> k -> IO ()
deleteS (UM umap0) key = 
  withForeignPtr umap0 $ \umap ->
  with key $ \pKey -> do
    hashmap_delete umap (castPtr pKey) (fromIntegral . sizeOf $ key)

foldMS :: (Storable k, Storable v) => (a -> (k, v) -> IO a) -> a -> UnorderedMap k v -> IO a
foldMS f acc0 (UM umap0) = withForeignPtr umap0 $ \umap -> do
  acc <- newIORef acc0
  it <- iter_create umap
  loop umap it acc
  iter_destroy it
  readIORef acc
    where 
      loop umap it acc = do
        hasNext <- iter_hasNext umap it
        if not hasNext
          then readIORef acc
          else
          with 2 $ \pNK ->
          with 2 $ \pNV ->
          with nullPtr $ \ppKey ->
          with nullPtr $ \ppVal -> do
            iter_next umap it ppKey pNK ppVal pNV
            nK <- peek pNK
            nV <- peek pNV
            key <- peek ppKey >>= peek . castPtr
            val <- peek ppVal >>= peek . castPtr
            readIORef acc >>= (`f` (key, val)) >>= writeIORef acc
            loop umap it acc

