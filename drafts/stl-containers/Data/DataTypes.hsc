{-# LANGUAGE ForeignFunctionInterface #-}

#include <bindings.dsl.h>
#include "hashmap.h"

module Data.DataTypes where

#strict_import

-- data UMO  -- unordered_map object
-- type UnorderedMap_ = ForeignPtr UMO
-- data UnorderedMap k v = UM {-# UNPACK #-} !UnorderedMap_
#opaque_t UMO

#ccall hashmap_create, IO (Ptr <HashMap>)

#ccall hashmap_destroy, FinalizerPtr <HashMap>

#ccall hashmap_insert, (Ptr <HashMap>) -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> IO ()

#ccall hashmap_lookup, (Ptr <HashMap>) -> Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()

#ccall hashmap_delete, (Ptr <HashMap>) -> Ptr Word8 -> CSize -> IO ()

#ccall hashmap_size, (Ptr <HashMap>) -> IO CSize

-- data UMIO -- unordered_map::iterator object
#opaque_t UMIO

#ccall iter_create, (Ptr <HashMap>) -> IO (Ptr <HashMapIter>)

#ccall iter_destory, FinalizerPtr <HashMapIter>

#ccall iter_hasNext, (Ptr <HashMap>) -> (Ptr <HashMapIter>) -> IO Bool

#ccall iter_next, (Ptr <HashMap>) -> (Ptr <HashMapIter>) -> Ptr (Ptr Word8) -> Ptr CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO Bool
