module HECFS where

import           Codec.Encryption.Blowfish (decrypt, encrypt)
import           Codec.Encryption.Padding  (pkcs5, unPkcs5)
import qualified Codec.FEC                 as F
import           Control.Monad
import           Data.Bits
import qualified Data.ByteString           as B
import qualified Data.ByteString.Lazy      as Bl
import qualified Data.Serialize            as S (decode, encode)
import           Data.Word

data Block = Block {getHeader :: Word8, getBody :: B.ByteString} deriving (Show)

padBit = bitSize (undefined :: Word8) - 1

isPadded :: Block -> Bool
isPadded (Block b _) = testBit b padBit

index :: Block -> Word8
index (Block b _) = clearBit b padBit

maxIndex :: Word8
maxIndex = (clearBit maxBound padBit)

makeBlock :: Bool -> Word8 -> B.ByteString -> Block
makeBlock padded index body
  | index > maxIndex = error $ "index should be <= " ++ show (maxIndex)
  | padded = Block (setBit index padBit) body
  | not padded = Block index body

-- encodeBlock :: Integral a => a -> Int -> Int -> B.ByteString -> [Block]
encodeBlock key k n src =
  F.encode (F.fec k n) . S.encode . map (encrypt key) . pkcs5 . B.unpack
  where blockSize = ceiling $ (fromIntegral $ B.length src) / k


encode :: Integral a => a -> Int -> Int -> Bl.ByteString -> [B.ByteString]
encode key k n = F.enFEC k n . S.encode . map (encrypt key) . pkcs5 . Bl.unpack

decode :: Integral a => a -> Int -> Int -> [B.ByteString] -> B.ByteString
decode key k n = B.pack . unPkcs5 . map (decrypt key) . either error id . S.decode . F.deFEC k n

store :: Integral a => a -> FilePath -> Int -> [FilePath] -> IO ()
store key src k trgs =
  Bl.readFile src >>= writeSplit . encode key k (length trgs)
  where
    writeSplit = zipWithM_ B.writeFile trgs

retrieve :: Integral a => a -> [FilePath] -> Int -> FilePath -> IO ()
retrieve key srcs k trg =
  readSplit >>= B.writeFile trg . decode key k (length srcs)
  where
    readSplit = mapM B.readFile srcs

toBlocks :: Int -> Bl.ByteString -> [B.ByteString]
toBlocks n bstr | Bl.null bstr = []
                | otherwise = let (block, rest) = Bl.splitAt (fromIntegral n) bstr
                              in Bl.toStrict block : toBlocks n rest
