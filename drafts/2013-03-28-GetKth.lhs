---
title: k번째 수 구하기
author: comatose
tags: haskell, binary search, quiz
---

x1, x2, x3 .. 각각의 배수로 이뤄진 집합에서 k번째 수를 구하는 방법

> import Data.List (foldl1', foldl')

1. 단순하게 처음부터 세는 방법 O(k)

> multiples n = map(*n) [1..]
  
> -- | merge sort with deduplication
> merge :: [Int] -> [Int]
> merge ns = (go $ map multiples ns)
>   where
>     go :: [[Int]] -> [Int]
>     go nss = let h = minimum $ map head nss
>              in h : (go $ map (dropWhile (<=h)) nss)

> -- | index begins from 1
> getKth :: [Int] -> Int -> Int
> getKth ns n = merge ns !! (n - 1)

2. 단조 증가 역함수를 구하고 이진 검색을 이용한 방법 O(log k)

> choose :: [a] -> Int -> [[a]]
> choose _ 0 = [[]]
> choose as0 n | n > length as0 = []
>              | otherwise = concatMap (\(a:as) -> map (a:) (choose as (n - 1)))
>                            . takeWhile (not . null) $ iterate tail as0

> -- | the inverse function of getKth
> -- i.d. getKth x == y --> countUpto y == x
> countUpto :: [Int] -> Int -> Int
> countUpto ns u = foldl' (\acc (op, n) -> acc `op` n) 0
>                  . zip (cycle [(+), (-)])
>                  . map (sum
>                         . map ((div u) . (foldl1' lcm))
>                         . choose ns)
>                  $ [1..length ns]

> -- | more efficient version of 'getKth'
> -- binary-search the lowest number 'i' which meets the countUpto i == n
> getKth' :: [Int] -> Int -> Int
> getKth' ns n = go 0 (n * minimum ns)
>   where go l u
>           | l == u = l
>           | otherwise = let m = (l + u) `div` 2
>                         in case compare (countUpto ns m) n of LT -> go (m + 1) u
>                                                               EQ -> go l m
>                                                               GT -> go l (m - 1)
