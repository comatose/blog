{-# LANGUAGE ForeignFunctionInterface #-}

#include <bindings.dsl.h>
#include "ffi.h"

module Data.DataTypes where

#strict_import

-- data UMO  -- unordered_map object
-- type UnorderedMap_ = ForeignPtr UMO
-- data UnorderedMap k v = UM {-# UNPACK #-} !UnorderedMap_
#opaque_t UMO

#ccall hashmap_create, IO (Ptr ())

#ccall hashmap_destroy, FunPtr(Ptr () -> IO ())

#ccall hashmap_insert, (Ptr ()) -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> IO ()

#ccall hashmap_lookup, (Ptr ()) -> Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()

#ccall hashmap_delete, (Ptr ()) -> Ptr Word8 -> CSize -> IO ()

#ccall hashmap_size, (Ptr ()) -> IO CSize

-- data UMIO -- unordered_map::iterator object
#opaque_t UMIO

#ccall iter_create, (Ptr ()) -> IO (Ptr ())

#ccall iter_destory, FunPtr(Ptr () -> IO ())

#ccall iter_hasNext, (Ptr ()) -> (Ptr ()) -> IO Bool

#ccall iter_next, (Ptr ()) -> (Ptr ()) -> Ptr (Ptr Word8) -> Ptr CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
