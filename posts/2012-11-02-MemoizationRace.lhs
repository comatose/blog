---
title: Memoization Benchmark
author: comatose
tags: haskell, memoization
---

> {-# LANGUAGE FlexibleContexts      #-}
> {-# LANGUAGE FlexibleInstances     #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
>
> module Main where
>
> import           Control.Exception
> import           Control.Monad
> import           Control.Monad.State
> import           Data.Array
> import           Data.Function
> import           Data.Hashable
> import qualified Data.HashMap.Lazy     as H
> import qualified Data.IntMap           as I
> import qualified Data.Map              as M
> import           System.CPUTime
> import           System.Environment
> import           Text.Printf

haskell은 memoization이 비교적 쉽게 가능한데, [hoogle]과 [hayoo]를 검색해 보니 총 4개의 package가 검색되었다.

[hoogle]:http://www.haskell.org/hoogle/
[hayoo]:http://holumbus.fh-wedel.de/hayoo/hayoo.html

1. [memoize](http://hackage.haskell.org/package/memoize)
2. [data-memocombinators](http://hackage.haskell.org/package/data-memocombinators)
3. [MemoTrie](http://hackage.haskell.org/package/MemoTrie)
4. [monad-memo](http://hackage.haskell.org/package/monad-memo)

> import qualified Data.Function.Memoize as M1
> import qualified Data.MemoCombinators  as M2
> import qualified Data.MemoTrie         as M3
> import qualified Control.Monad.Memo    as M4

fibonacci 수열을 대상으로 간단한 benchmark를 수행해 보았다.

> instance (Eq k, Hashable k) => M4.MapLike (H.HashMap k v) k v where
>   lookup = H.lookup
>   add = H.insert

> fib :: Int -> Integer
> fib 0 = 1
> fib 1 = 1
> fib n = fib (n - 1) + fib (n - 2)

> fibG :: (Int -> Integer) -> Int -> Integer
> fibG fib 0 = 1
> fibG fib 1 = 1
> fibG fib n = fib (n - 1) + fib (n - 2)

`figG`는 open recursion으로 정의되었다.

> fibMC :: Int -> Integer
> fibMC = M2.integral go
>   where go 0 = 1
>         go 1 = 1
>         go n = fibMC (n - 1) + fibMC (n - 2)

`figMC`는 MemoCombinator 사용

> fibMM :: (M4.MapLike c Int Integer) => c -> Int -> Integer
> fibMM ml = (`M4.evalMemoState` ml) . f
>   where f 0 = return 1
>         f 1 = return 1
>         f n = liftM2 (+) (M4.memo f (n - 1)) (M4.memo f (n - 2))

`fibMM`는 monad-memo package 사용

> fibST :: (M4.MapLike c Int Integer) => c -> Int -> Integer
> fibST ml n0 = evalState (go n0) ml
>   where go 0 = return 1
>         go 1 = return 1
>         go n = do
>           memo <- get
>           case M4.lookup n memo of
>             Just v -> return v
>             Nothing -> do
>               v <- liftM2 (+) (go (n - 1)) (go (n - 2))
>               modify (M4.add n v)
>               return v

`fibST`는 `MapLike` instance와 `State` monad를 이용한 memoization

> fibL :: Int -> Integer
> fibL n0 = [f i | i <- [0..n0]] !! n0
>   where f 0 = 1
>         f 1 = 1
>         f n = fibL (n - 1) + fibL (n - 2)
>
> fibA :: Int -> Integer
> fibA n0 = array (0, n0) [(i, f i) | i <- [0..n0]] ! n0
>   where f 0 = 1
>         f 1 = 1
>         f n = fibA (n - 1) + fibA (n - 2)

`fibL`과 `fibA`는 각각 `List`와 `Array`에 lazy-evaluation 사용

> fibIt :: Int -> Integer
> fibIt n0 | n0 < 2 = 1
>          | otherwise = go 2 1 1
>   where go n a b | n == n0 = a + b
>                  | otherwise = go (n + 1) (a + b) a

`fibIt`는 tail-recursive iteration 구현

> main = do
>   n <- liftM (read. head) getArgs
>   vs <- mapM (time . ($n)) [

Fujitsu P1620 랩탑에서 100000번째 fibonacci 수를 구하는데 걸리는 시간을 재봤다.

>     M1.memoize fib   -- worst, same performance as fib

`memoize`는 거의 사용 불가, `fib`와 거의 같은 성능을 보인다.

>     M1.memoFix fibG, -- memoize

`memofix`는 3.432s

>     fibMC, -- data-memocombinators

`fibMC`는 6.605s

>     fix (M3.memo . fibG) --MemoTrie

이건 4.764s

>     fibMM I.empty, -- "monad-memo" using IntMap
>     fibMM H.empty, -- "monad-memo" using HashMap
>     fibMM M.empty, -- "monad-memo" using Map

1.252s, 1.460s, 1.492s

>     fibST I.empty, -- state monad using IntMap
>     fibST H.empty, -- state monad using HashMap
>     fibST M.empty, -- state monad using Map

1.620s, 2.012s, 2.304s

>     fibL, fibA, -- lazy evaluation with List and Array

이것도 사용 불가, Array를 써도 그다지 성능 향상이 없다.

>     fibIt -- tail-recursive iteration

0.252s, 비교 불가.

>     ]
>   end <- getCPUTime
>   print $ all (== head vs) $ tail vs
>   printf "%0.9f sec \n" ((fromIntegral (end - start) :: Double) / 10^12)
>   return v
>   start <- getCPUTime
>   v <- evaluate m
> time m = do

최종 순위

1. 0.252s, iteration
2. 1.252s, monad-memo
3. 1.620s, state monad
4. 3.432s, memoize
5. 4.764s, memo-trie
6. 6.605s, data-memocombinators

종합적으로 고려해 볼 때 monad-memo가 memoization package 중에서 가장 좋은 성능을 보인다.