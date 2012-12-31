{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Codec.FEC                (deFEC, enFEC)
import           Control.Monad
import           Control.Monad.ST         (runST)
import           Crypto.Cipher.AES        (Key, decrypt, encrypt, initKey256)
import           Crypto.Hash.SHA256       (hash)
import           Crypto.Padding           (padPKCS5, unpadPKCS5)
import qualified Data.ByteString          as B
import           Data.ByteString.Char8    (pack)
import qualified Data.Conduit             as C
import qualified Data.Conduit.List        as CL
import           Data.Maybe               (fromJust)
import           System.Console.Haskeline
import           System.Environment       (getArgs)
import qualified Text.JSON.Generic        as J

data Config = Config{ nodeSet :: [FilePath], numPrimaryNodes :: Int } deriving (J.Typeable, J.Data, Show)

main :: IO ()
main = do
  key <- runInputT defaultSettings (getPassword (Just '*') "Password: ") >>= createKey . fromJust
  (Config nodes k) <- readConf "conf.json"
  [op, fn] <- getArgs
  case op of
    "en" -> store key fn k (length nodes)
    "de" -> retrieve key fn k (length nodes)
    _ -> error "invalid option"

createKey :: String -> IO Key
createKey = either error return . initKey256 . hash . pack

readConf :: FilePath -> IO Config
readConf fn = do
  str <- readFile fn
  either error return $
    J.resultToEither (J.decode str >>= J.fromJSON)

store :: Key -> FilePath -> Int -> Int -> IO ()
store key fn k n =
  B.readFile fn >>= writeSplit . enFEC k n . encrypt key . padPKCS5 16
  where writeSplit = zipWithM_ B.writeFile [fn ++ "." ++ show num | num <- ([0..] :: [Int])]

retrieve :: Key -> FilePath -> Int -> Int -> IO ()
retrieve key fn k n =
  readSplit >>= B.writeFile (fn ++ ".dec") . unpadPKCS5 . decrypt key . deFEC k n
  where readSplit = sequence [B.readFile (fn ++ "." ++ show num) | num <- ([1..(n - 1)] :: [Int])]

