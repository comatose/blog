module Haksup.Clustering.KMeans
       (
         kmeansGen
         , kmeans
         , Point
       )
       where

-- import Debug.Trace
import           Control.Monad               (liftM2)
import           Control.Parallel.Strategies
import           Data.Function               (on)
import           Data.List                   (foldl1', minimumBy)
import qualified Data.Map                    as M

type Point = [Double]

cluster :: (NFData a) => (a -> Point) -> [Point] -> [a] -> [[a]]
-- cluster f cs = M.elems . M.fromListWith (++) . map (\x -> (closestOf x, [x]))
cluster f cs = M.elems . M.fromListWith (++) . withStrategy (parList rdeepseq) . map (\x -> (closestOf x, [x]))
  where
    closestOf x = minimumBy (compare `on` distance (f x)) cs
    distance x y = sum . (map (^2)) $ zipWith (-) x y

centroidOf :: [Point] -> Point
centroidOf = liftM2 (divide) (foldl1' plus) (fromIntegral . length)
  where plus x y = zipWith (+) x y
        divide x n = map (/n) x

kmeansGen :: (NFData a) => (a -> Point) -> Int -> [a] -> [[a]]
kmeansGen f k xs = loop (take k . map f $ xs)
  where loop cs = let clst = cluster f cs xs
                      -- ncs = map (centroidOf . map f) clst
                      ncs = withStrategy (parList rdeepseq) . map (centroidOf . map f) $ clst
                  in if cs == ncs then clst else loop ncs

kmeans :: Int -> [Point] -> [[Point]]
kmeans = kmeansGen id
