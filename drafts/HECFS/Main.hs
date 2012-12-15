{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Codec.FEC             as EC
import           Control.Monad
import qualified Data.ByteString       as B
import           Data.ByteString.Char8 (pack)
import           System.Environment
import qualified Text.JSON.Generic     as J
import Control.Monad (liftM)

data Config = Config{ nodeSet :: [FilePath], numPrimaryNodes :: Int } deriving (J.Typeable, J.Data, Show)

main :: IO ()
main = do
  key <- getLine >>= liftM pack
  (Config nodes k) <- readConf "conf.json"
  [op, fn] <- getArgs
  case op of
    "en" -> encrypt fn k (length nodes)
    "de" -> decrypt fn k (length nodes)
    _ -> error "invalid option"

readConf :: FilePath -> IO Config
readConf fn = do
  str <- readFile fn
  either error return $
    J.resultToEither (J.decode str >>= J.fromJSON)

encrypt :: String -> Int -> Int -> IO ()
encrypt fn k n =
  B.readFile fn >>= writeSplit . EC.enFEC k n
    where writeSplit = zipWithM_ B.writeFile [fn ++ "." ++ show num | num <- ([0..] :: [Int])]

decrypt :: String -> Int -> Int -> IO ()
decrypt fn k n =
  readSplit >>= B.writeFile (fn ++ ".dec") . EC.deFEC k n
    where readSplit = sequence [B.readFile (fn ++ "." ++ show num) | num <- ([1..(n - 1)] :: [Int])]

