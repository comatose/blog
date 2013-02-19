{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

import Data.Monoid (mempty)
import Test.Framework (defaultMain, defaultMainWithOpts, testGroup)
import Test.Framework.Options (TestOptions, TestOptions'(..))
import Test.Framework.Runners.Options (RunnerOptions, RunnerOptions'(..))
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.TH

import Test.QuickCheck
import Test.HUnit

import Data.List

import Haksup.Clustering.KMeans
import qualified Data.ByteString.Lazy as Bl
import Data.Int (Int32)
import Data.Serialize (decodeLazy)

main = $(defaultMainGenerator)-- defaultMain tests

tests = [
        testGroup "Group 'kmeans'" [
                testProperty "kmeans1" prop_kmeans1
                , testProperty "kmeans2" prop_kmeans2
                ]
        , testGroup "Group 'kmeansGen'" [
                testProperty "kmeansGen1" prop_kmeansGen1
                , testProperty "kmeansGen2" prop_kmeansGen2
                ]
        ]


newtype Samples a = Samples (Int, [a]) deriving (Show, Read)
  
instance Arbitrary (Samples Point) where
  arbitrary = fmap Samples $ do
    k <- choose (4, 10)
    d <- choose (16, 128)
    xs <- listOf1 . vector $ d
    return (k, xs)

prop_kmeans1 (Samples (k, xs)) =
  sort xs == (sort . concat . kmeans k $ xs)

prop_kmeans2 (Samples (k, xs)) =
  (k >= length (kmeans k xs))

instance Arbitrary (Samples Bl.ByteString) where
  arbitrary = fmap Samples $ do
    k <- choose (4, 10)
    d <- choose (16, 128)
    xs <- listOf1 . fmap (Bl.pack) . vector $ d
    return (k, xs)

toPoint :: Bl.ByteString -> Point
toPoint = map (fromIntegral . toInt32) . toBlocks 4
  where toInt32 :: Bl.ByteString -> Int32
        toInt32 = either (const 0) id . decodeLazy
        toBlocks :: Int -> Bl.ByteString -> [Bl.ByteString]
        toBlocks n bstr | Bl.null bstr = []
                        | otherwise = let (block, rest) = Bl.splitAt (fromIntegral n) bstr
                                      in block : toBlocks n rest

prop_kmeansGen1 (Samples (k, xs)) =
  sort xs == (sort . concat . kmeansGen toPoint k $ xs)

prop_kmeansGen2 (Samples (k, xs)) =
  (k >= length (kmeansGen toPoint k xs))
