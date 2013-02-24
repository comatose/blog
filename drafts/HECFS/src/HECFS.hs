module HECFS where

import           Codec.Encryption.Blowfish (decrypt, encrypt)
import           Codec.Encryption.Padding  (pkcs5, unPkcs5)
import           Codec.FEC                 (deFEC, enFEC)
import           Control.Monad
import qualified Data.ByteString           as B
import qualified Data.ByteString.Lazy      as Bl
import           Data.Int
import           Data.Maybe                (fromJust)
import qualified Data.Serialize            as S (decode, encode)

toBlocks :: Int -> Bl.ByteString -> [B.ByteString]
toBlocks n bstr | Bl.null bstr = []
                | otherwise = let (block, rest) = Bl.splitAt (fromIntegral n) bstr
                              in Bl.toStrict block : toBlocks n rest

store :: Integral a => a -> FilePath -> Int -> [FilePath] -> IO ()
store key src k trgs =
  Bl.readFile src >>= writeSplit . encode
  where
    writeSplit = zipWithM_ B.writeFile trgs
    encode :: Bl.ByteString -> [B.ByteString]
    encode = enFEC k (length trgs) . S.encode . map (encrypt key) . pkcs5 . Bl.unpack

-- [fn ++ "." ++ show num | num <- ([0..] :: [Int])]

retrieve :: Integral a => a -> [FilePath] -> Int -> FilePath -> IO ()
retrieve key srcs k trg =
  readSplit >>= B.writeFile trg . decode
  where
    readSplit = mapM B.readFile srcs
    decode :: [B.ByteString] -> B.ByteString
    decode = B.pack . unPkcs5 . map (decrypt key) . either error id . S.decode . deFEC k (length srcs)

-- (fn ++ "." ++ show num) | num <- ([1..(n - 1)] :: [Int])
