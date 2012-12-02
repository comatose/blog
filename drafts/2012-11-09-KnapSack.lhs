> module Main where
>
> import Data.Array
> import Control.Arrow

> data Item = Item {value :: Int, size :: Int} deriving (Ord, Eq, Show)
> 
> type Cell = (Int, [Item])

> dynPack :: Int -> [Item] -> Cell
> dynPack _ [] = (0, [])
> dynPack packSize items@(i:is)
>   | packSize < 1 = (0, [])
>   | otherwise = let (v1, s1) = dynPack packSize is
>                     (v2, s2) = if packSize < size i
>                                then (0, [])
>                                else first (+ value i) (dynPack (packSize - size i) is)
>                                -- else first (+ value i) (dynPack (packSize - size i) items)  -- in the case of unbo
>                                     
>                 in if v1 < v2
>                    then (v2, i:s2)
>                    else (v1, s1)

> memoPack :: Int -> [Item] -> Cell
> memoPack packSize items = table ! (packSize, numItems)
>   where
>     numItems = length items
>     arrItems = listArray (1, numItems) items
>     sizeAt = size . (arrItems !)
>     valueAt = value . (arrItems !)
>     cellAt (i, j) | i < 1 || j < 1 = (0, [])
>                   | otherwise = table ! (i, j)
>     table = array ((1, 1), (packSize, numItems)) [((i, j), cell i j) | i <- [1..packSize], j <- [1..numItems]]
>     cell i j = let (v1, s1) = cellAt (i, j - 1)
>                    (v2, s2) = if i < sizeAt j
>                               then (0, [])
>                               else first (+ valueAt j) (cellAt (i - sizeAt j, j - 1))
>                in if v1 < v2
>                   then (v2, (arrItems ! j):s2)
>                   else (v1, s1)