module Haksup.Clustering.KMeans where

-- import Debug.Trace
import qualified Data.Map as M
import Control.Arrow ((&&&))
import Control.Parallel.Strategies

class (Show c, Ord c, Eq c) => Coordinate c where
  distance :: c -> c -> Double
  centroid :: [c] -> c

cluster :: (NFData c, Coordinate c) => [c] -> [c] -> [[c]]
cluster cs = M.elems . M.fromListWith (++) . map (\x -> (closest x, [x]))
  where closest x = snd . minimum . map (distance x &&& id) $ cs
-- cluster cs = M.elems . M.fromListWith (++) . withStrategy (parListChunk 2500 rdeepseq) . (map (\x -> (closest x, [x])))

kmeans :: (NFData c, Coordinate c) => Int -> [c] -> [[c]]
kmeans n xs = go (take n xs)
  where go cs = let clst = cluster cs xs
                    ncs = map centroid clst
                    -- ncs = withStrategy (parList rdeepseq) (map centroid clst)
                in if ncs == cs
                   then clst
                   else go ncs
