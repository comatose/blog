{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PackageImports            #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module HECFS.Proxy where

import           Codec.FEC                (deFEC, enFEC)
import           Control.Exception        (bracket)
import           Control.Monad
import           Control.Proxy            ((>->))
import qualified Control.Proxy            as P
import qualified Control.Proxy.ByteString as PB
import           "cryptocipher" Crypto.Cipher.AES        (Key, decrypt, encrypt,
                                                          initKey256)
import           Crypto.Hash.SHA256       (hash)
import           Crypto.Padding           (padPKCS5, unpadPKCS5)
import qualified Data.ByteString          as B
import           Data.ByteString.Char8    (pack)
import           Data.Maybe               (fromJust)
import           System.IO

store :: Key -> FilePath -> Int -> [FilePath] -> IO ()
store key src k trgs =
-- store :: Key -> FilePath -> Int -> Int -> IO ()
-- store key fn k n =
  bracket openFiles closeFiles process
  where
    openFiles = do
      ih <- openFile src ReadMode
      ohs <- mapM (`openFile` WriteMode) trgs
      return (ih, ohs)
    closeFiles (ih, ohs) = mapM_ hClose (ih:ohs)
    process (ih, ohs) = P.runProxy $ PB.hGetSomeS 1023 ih >-> P.mapD encode >-> P.mapMD (writeSplit ohs)
    writeSplit :: [Handle] -> [B.ByteString] -> IO ()
    writeSplit = zipWithM_ B.hPut
    encode :: B.ByteString -> [B.ByteString]
    encode = enFEC k (length trgs) . encrypt key . padPKCS5 16

retrieve :: Key -> [FilePath] -> Int -> FilePath -> IO ()
retrieve key srcs k trg =
  bracket openFiles closeFiles process
  where
    openFiles = do
      ihs <- mapM (`openFile` ReadMode) srcs
      oh <- openFile trg WriteMode
      return (ihs, oh)
    closeFiles (ihs, oh) = mapM_ hClose (oh:ihs)
    process (ihs, oh) = P.runProxy $ readSplit ihs >-> P.mapD decode >-> PB.toHandleD oh
    readSplit ihs () = P.runIdentityP go where
      go = do
        eof <- liftM or (mapM (P.lift . hIsEOF) ihs)
        if eof
          then return ()
          else do bs <- mapM (P.lift . (`B.hGetSome` 1024)) ihs
                  P.respond bs
                  go
    decode :: [B.ByteString] -> B.ByteString
    decode = unpadPKCS5 . decrypt key . deFEC k (length srcs)
