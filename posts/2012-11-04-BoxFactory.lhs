---
title: BoxFactory
author: comatose
tags: GCJ, haskell
---
[Problem Description](http://code.google.com/codejam/contest/1781488/dashboard#s=p2)

> {-# LANGUAGE FlexibleInstances     #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> 
> module Main where
> 
> import           Control.Monad
> import           Control.Monad.Memo
> import           Control.Monad.State
> import           Data.Hashable
> import qualified Data.HashMap.Lazy           as H
> import qualified Data.Map                    as M

> type Problem = ([(Integer, Int)], [(Integer, Int)])

> solveNaive :: Problem -> String
> solveNaive = show . uncurry go
>   where
>     go ball@((n, b):bs) tall@((m, t):ts)
>       = if b == t
>         then if n > m
>              then m + go ((n - m, b):bs) ts
>              else n + go bs ((m - n, t):ts)
>         else
>           max (go ball ts) (go bs tall)
>     go _ _ = 0

> instance (Eq k, Hashable k) => MapLike (H.HashMap k v) k v where
>   lookup = H.lookup
>   add = H.insert

> solveMM mc = show . (`evalMemoState` mc) . (uncurry . fix $ for2 memo . goM)
>   where
>     goM f ball@((n, b):bs) tall@((m, t):ts)
>       = if b == t
>         then if n > m
>              then liftM (m+) $ f ((n - m, b):bs) ts
>              else liftM (n+) $ f bs ((m - n, t):ts)
>         else
>           liftM2 max (f ball ts) (f bs tall)
>     goM _ _ _ = return 0

> main :: IO ()
> main = interact $
>        unlines . numbering . map (solveMM H.empty)
>        . problemify . tail . lines
>     where
>       numbering :: [String] -> [String]
>       numbering = zipWith (++) ["Case #" ++ show n ++ ": " | n <- ([1..] :: [Int])]
> 
>       problemify :: [String] -> [Problem]
>       problemify (_:boxes:toys:rest) = (parsePair boxes, parsePair toys) : problemify rest
>       problemify _ = []
> 
>       chunk :: Int -> [a] -> [[a]]
>       chunk _ [] = []
>       chunk n xs = c : chunk n rest where (c, rest) = splitAt n xs
> 
>       parsePair :: String -> [(Integer, Int)]
>       parsePair = foldr merge [] . map listToIntPair . chunk 2 . words
> 
>       merge x [] = [x]
>       merge (n, t) a@((acn, act):arest) = if t == act then ((acn + n), act):arest else (n, t):a
> 
>       listToIntPair [i, j] = (read i, read j)
