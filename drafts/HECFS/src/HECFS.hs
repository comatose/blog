module HECFS where

import           Codec.Encryption.Blowfish (decrypt, encrypt)
import           Codec.Encryption.Padding  (pkcs5, unPkcs5)
import           Codec.Utils               (listToOctets, listFromOctets)
import qualified Codec.FEC                 as F
import           Control.Monad
import qualified Data.ByteString           as B

encode :: Integral a => a -> Int -> Int -> B.ByteString -> [B.ByteString]
encode key k n = F.enFEC k n . B.pack . listToOctets . map (encrypt key) . pkcs5 . B.unpack

decode :: Integral a => a -> Int -> Int -> [B.ByteString] -> B.ByteString
decode key k n = B.pack . unPkcs5 . map (decrypt key) . listFromOctets . B.unpack . F.deFEC k n

store :: Integral a => a -> FilePath -> Int -> [FilePath] -> IO ()
store key src k trgs =
  B.readFile src >>= writeSplit . encode key k (length trgs)
  where
    writeSplit = zipWithM_ B.writeFile trgs

retrieve :: Integral a => a -> [FilePath] -> Int -> FilePath -> IO ()
retrieve key srcs k trg =
  readSplit >>= B.writeFile trg . decode key k (length srcs)
  where
    readSplit = mapM B.readFile srcs
