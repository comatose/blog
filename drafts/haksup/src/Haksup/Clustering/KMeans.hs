module Haksup.Clustering.KMeans
       (
         kmeansGen
         , kmeans
       )
       where

-- import Debug.Trace
import qualified Data.Map as M
import Control.Arrow ((&&&))
import Control.Parallel.Strategies
import Data.List (foldl1', minimumBy)
import Data.Function (on)
import Control.Monad (liftM2)

type Point = [Double]

cluster :: (a -> Point) -> [Point] -> [a] -> [[a]]
cluster f cs = M.elems . M.fromListWith (++) . map (\x -> (closest x, [x]))
-- cluster cs = M.elems . M.fromListWith (++) . withStrategy (parListChunk 2500 rdeepseq) . (map (\x -> (closest x, [x])))
  where
    closest x = snd . minimumBy (compare `on` fst) . map (distance x &&& id) $ cs
    distance x y = sum . (map (^2)) $ zipWith (-) (f x) y

centroidOf :: [Point] -> Point
centroidOf = liftM2 (divide) (foldl1' plus) (fromIntegral . length)
  where plus x y = zipWith (+) x y
        divide x n = map (/n) x
                    
kmeansGen :: (a -> Point) -> Int -> [a] -> [[a]]
kmeansGen f k xs = loop (take k . map f $ xs)
  where loop cs = let clst = cluster f cs xs
                      ncs = map (centroidOf . map f) clst
                    -- ncs = withStrategy (parList rdeepseq) (map (centroidOf . map f) clst)
                  in if cs == ncs then clst else loop ncs

kmeans :: Int -> [Point] -> [[Point]]
kmeans = kmeansGen id
