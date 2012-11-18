---
title: Shifting Paths
author: comatose
tags: GCJ, haskell
---

[Problem Description](http://code.google.com/codejam/contest/2075486/dashboard#s=p4)

여전히 solveLarge는 미해결.

> import           Control.Parallel.Strategies
> import           Data.Array
> import qualified Data.BitSet                 as B

> -- left and right paths
> type Clearing = (Int, Int)
> type VisitState = B.BitSet Int
> data Problem = Problem {destiny :: Int, clearings :: (Array Int Clearing)}
>              deriving Show

> data Scene = Scene {passed :: !Integer, clearing :: Int, state :: !VisitState}
> instance Eq Scene where
>   (Scene _ c1 s1) == (Scene _ c2 s2) = c1 == c2 && s1 == s2
>   -- (==) = (==) `on` (clearing &&& state)

> forward :: Problem -> Scene -> Scene
> forward (Problem dest clrs) (Scene c i m)
>   | i == dest = Scene c i m
>   | B.member i m = Scene (c + 1) (snd path) (B.delete i m)   -- even case
>   | otherwise = Scene (c + 1) (fst path) (B.insert i m)    -- odd case
>   where path = clrs ! i

> -- memory-intensive, only practical for small problems
> solveSmall :: Problem -> String
> solveSmall p@(Problem dest _) = if or $ zipWith (==) trail (ffw2x trail)
>                                 then "Infinity"
>                                 else show . length $ trail
>   where
>     -- series of scenes just before destination
>     trail = takeWhile ((<dest). clearing) . iterate (forward p) $ (Scene 0 1 B.empty)
>     -- fast forward 2x
>     ffw2x :: [a] -> [a]
>     ffw2x (_:x:xs) = x:ffw2x xs
>     ffw2x _ = []

> -- constant memory consumption, at the expense of computation
> solveLarge :: Problem -> String
> solveLarge p@(Problem dest _) = go (Scene 0 1 B.empty) (forward p (Scene 0 1 B.empty))
>   where
>     go :: Scene -> Scene -> String
>     go x1 x2
>       | clearing x2 == dest = show . passed $ x2
>       | x1 == x2 = "Infinity"
>       | otherwise = go (forward p x1) (forward p . forward p $ x2)

> main :: IO ()
> main = interact $
>        unlines . withStrategy (parList rdeepseq) . numbering
>        . map solveLarge . problemify . tail . lines
>     where
>       numbering :: [String] -> [String]
>       numbering = zipWith (++) ["Case #" ++ show n ++ ": " | n <- ([1..] :: [Int])]
>
>       -- build a clearing from one line
>       parseClearing :: String -> Clearing
>       parseClearing xs = let [l, r] = map read $ words xs in (l, r)
>
>       problemify :: [String] -> [Problem]
>       problemify [] = []
>       problemify (x:xs) =
>         Problem n (listArray (1, n - 1) (map parseClearing clrs)) : problemify rest
>         where n = read x
>               (clrs, rest) = splitAt (n - 1) xs
