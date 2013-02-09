{-# LANGUAGE PackageImports #-}

module HECFS where

import qualified Codec.FEC                as FEC (deFEC, enFEC)
import           Control.Monad
import           Control.Monad.IO.Class
import           "cryptocipher" Crypto.Cipher.AES        (Key, decrypt, encrypt,
                                                          initKey256)
import           Crypto.Hash.SHA256       (hash)
import           Crypto.Padding           (padPKCS5, unpadPKCS5)
import qualified Data.ByteString          as B
import           Data.ByteString.Char8    (pack)
import           Data.Conduit
import qualified Data.Conduit.Binary      as CB
import qualified Data.Conduit.List        as CL
import           Data.Maybe               (fromJust)
import           System.Console.Haskeline
import qualified System.IO                as IO

sourceFile :: MonadResource m => Int -> FilePath -> GSource m B.ByteString
sourceFile n fp = sourceIOHandle n (IO.openBinaryFile fp IO.ReadMode)
  where
    sourceHandle n h = loop
      where loop = do
              bs <- liftIO (B.hGetSome h n)
              if B.null bs
                then return ()
                else yield bs >> loop
    sourceIOHandle n alloc = bracketP alloc IO.hClose (sourceHandle n)

enFEC :: (Monad m) => Int -> Int -> GInfConduit B.ByteString m [B.ByteString]
enFEC k n = CL.map (FEC.enFEC k n)

-- merge :: (Monad m) => [Pipe l i o u m r] -> Pipe [l] [i] [o] m [r]


store :: Key -> FilePath -> Int -> [FilePath] -> IO ()
store key src k trgs =
  B.readFile src >>= writeSplit . encode
  where
    writeSplit = zipWithM_ B.writeFile trgs
    encode = FEC.enFEC k (length trgs) . encrypt key . padPKCS5 16

-- store :: Key -> FilePath -> Int -> [FilePath] -> IO ()
-- store key src k trgs =
--   B.readFile src >>= writeSplit . encode
--   where
--     writeSplit = zipWithM_ B.writeFile trgs
--     encode = enFEC k (length trgs) . encrypt key . padPKCS5 16

-- retrieve :: Key -> [FilePath] -> Int -> FilePath -> IO ()
-- retrieve key srcs k trg =
--   readSplit >>= B.writeFile trg . decode
--   where
--     readSplit = mapM B.readFile srcs
--     decode = unpadPKCS5 . decrypt key . deFEC k (length srcs)
