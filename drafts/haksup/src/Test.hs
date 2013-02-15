{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Haksup.Clustering.KMeans
import Data.List
import Control.Monad (liftM2)

type Element = [Double]

instance Coordinate Element where
  distance xs ys = sum . (map (^2)) $ zipWith (-) xs ys
  centroid = liftM2 (divide) (foldl1' plus) (fromIntegral . length)
    where
      plus = zipWith (+)
      divide xs n = map (/n) xs

test :: [[Element]]
test = kmeans 4 [[x, y] | x <- [0..100], y  <- [0..100]]

main :: IO ()
main = print test
