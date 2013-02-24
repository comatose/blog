{-# LANGUAGE PackageImports #-}

module HECFS where

import           Codec.FEC                (deFEC, enFEC)
import           Control.Monad
import           "cryptocipher" Crypto.Cipher.AES        (Key, decrypt, encrypt,
                                                          initKey256)
import           Crypto.Hash.SHA256       (hash)
import           Crypto.Padding           (padPKCS5, unpadPKCS5)
import qualified Data.ByteString          as B
import           Data.ByteString.Char8    (pack)
import           Data.Maybe               (fromJust)
import           System.Console.Haskeline

store :: Key -> FilePath -> Int -> [FilePath] -> IO ()
store key src k trgs =
  B.readFile src >>= writeSplit . encode
  where
    writeSplit = zipWithM_ B.writeFile trgs
    encode = enFEC k (length trgs) . encrypt key . padPKCS5 16

-- [fn ++ "." ++ show num | num <- ([0..] :: [Int])]

retrieve :: Key -> [FilePath] -> Int -> FilePath -> IO ()
retrieve key srcs k trg =
  readSplit >>= B.writeFile trg . decode
  where
    readSplit = mapM B.readFile srcs
    decode = unpadPKCS5 . decrypt key . deFEC k (length srcs)

-- (fn ++ "." ++ show num) | num <- ([1..(n - 1)] :: [Int])
