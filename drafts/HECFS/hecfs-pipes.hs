{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PackageImports            #-}
{-# LANGUAGE ScopedTypeVariables       #-}

import           Codec.FEC                     (deFEC, enFEC)
import           Control.Monad
import           Control.Proxy                 ((>->))
import qualified Control.Proxy                 as P
import qualified Control.Proxy.ByteString      as PB
import           "cryptocipher" Crypto.Cipher.AES             (Key, decrypt,
                                                               encrypt,
                                                               initKey256)
import           Crypto.Hash.SHA256            (hash)
import           Crypto.Padding                (padPKCS5, unpadPKCS5)
import qualified Data.ByteString               as B
import           Data.ByteString.Char8         (pack)
import           Data.Maybe                    (fromJust)
import           System.Console.Haskeline
import           System.Environment            (getArgs)
import           System.IO
import qualified Text.JSON.Generic             as J

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
  bracket openFiles closeFiles process
  where
    openFiles = do
      ih <- openFile fn ReadMode
      ohs <- mapM (`openFile` WriteMode) [fn ++ "." ++ show num | num <- ([0..(n-1)] :: [Int])]
      return (ih, ohs)
    closeFiles (ih, ohs) = mapM_ hClose (ih:ohs)
    process (ih, ohs) = P.runProxy $ PB.hGetSomeS 1023 ih >-> P.mapD encode >-> P.mapMD (writeSplit ohs)
    writeSplit :: [Handle] -> [B.ByteString] -> IO ()
    writeSplit = zipWithM_ B.hPut
    encode :: B.ByteString -> [B.ByteString]
    encode = enFEC k n . encrypt key . padPKCS5 16

retrieve :: Key -> FilePath -> Int -> Int -> IO ()
retrieve key fn k n =
  bracket openFiles closeFiles process
  where
    openFiles = do
      ihs <- mapM (`openFile` ReadMode) [fn ++ "." ++ show num | num <- ([0..(n-1)] :: [Int])]
      oh <- openFile (fn ++ ".dec") WriteMode
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
    decode = unpadPKCS5 . decrypt key . deFEC k n
