module Haksup.Clustering.KMeans where

-- import Debug.Trace
import qualified Data.Map as M
import Control.Arrow ((&&&))
import Control.Parallel.Strategies
import Data.List (foldl1')
import Control.Monad (liftM2)

type Point = [Double]

cluster :: [Point] -> [Point] -> [[Point]]
cluster cs = M.elems . M.fromListWith (++) . map (\x -> (closest x, [x]))
-- cluster cs = M.elems . M.fromListWith (++) . withStrategy (parListChunk 2500 rdeepseq) . (map (\x -> (closest x, [x])))
  where closest x = snd . minimum . map (distance x &&& id) $ cs
        distance xs ys = sum . (map (^2)) $ zipWith (-) xs ys
                    

kmeans :: Int -> [Point] -> [[Point]]
kmeans n xs = go (take n xs)
  where go cs = let clst = cluster cs xs
                    ncs = map centroid clst
                    -- ncs = withStrategy (parList rdeepseq) (map centroid clst)
                in if ncs == cs
                   then clst
                   else go ncs
        centroid = liftM2 (divide) (foldl1' plus) (fromIntegral . length)
        plus = zipWith (+)
        divide xs n = map (/n) xs
