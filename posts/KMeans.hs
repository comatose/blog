{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Debug.Trace
import qualified Data.Map as M
import Data.List
import Control.Arrow ((&&&))
import Control.Monad (liftM2)
import Control.Parallel.Strategies

class (Show c, Ord c, Eq c) => Coord c where
  distance :: c -> c -> Double
  centroid :: [c] -> c

type Element = [Double]

instance Coord Element where
  distance xs ys = sqrt . sum . (map (^2)) $ zipWith (-) xs ys
  centroid = liftM2 (divide) (foldl1' plus) (fromIntegral . length)
    where
      plus = zipWith (+)
      divide xs n = map (/n) xs

cluster :: (NFData c, Coord c) => [c] -> [c] -> [[c]]
-- cluster cs = M.elems . M.fromListWith (++) . map (\x -> (closest x, [x]))
cluster cs = M.elems . M.fromListWith (++) . withStrategy (parListChunk 2500 rdeepseq) . (map (\x -> (closest x, [x])))
  where closest x = snd . minimum . map (distance x &&& id) $ cs

kmeans :: (NFData c, Coord c) => Int -> [c] -> [[c]]
kmeans n xs0 = go (take n xs0)
  where go cs = let clst = cluster cs xs0
                    -- ncs = map centroid clst
                    ncs = withStrategy (parList rdeepseq) (map centroid clst)
                in if ncs == cs
                   then clst
                   else go ncs

test :: [[Element]]
test = kmeans 4 [[x, y] | x <- [0..100], y  <- [0..100]]

main :: IO ()
main = print test
