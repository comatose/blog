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

`figG`는 open recursion으로 정의되었다.

> fibG :: (Int -> Integer) -> Int -> Integer
> fibG fib 0 = 1
> fibG fib 1 = 1
> fibG fib n = fib (n - 1) + fib (n - 2)

`figMC`는 MemoCombinator 사용

> fibMC :: Int -> Integer
> fibMC = M2.integral go
>   where go 0 = 1
>         go 1 = 1
>         go n = fibMC (n - 1) + fibMC (n - 2)

`fibMM`는 monad-memo package 사용

> fibMM :: (M4.MapLike c Int Integer) => c -> Int -> Integer
> fibMM ml = (`M4.evalMemoState` ml) . f
>   where f 0 = return 1
>         f 1 = return 1
>         f n = liftM2 (+) (M4.memo f (n - 1)) (M4.memo f (n - 2))

`fibST`는 `MapLike` instance와 `State` monad를 이용한 memoization

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

`fibCM`은 [여기](http://comatose.github.com/blog/_site/posts/2011-11-25-Memoization.html) 참고

> fibCM :: (M4.MapLike c Int Integer) => c -> Int -> Integer
> fibCM ml = (`evalState` ml) . (fix $ memoM fibMG)
>   where
>     fibMG :: (Monad m) => (Int -> m Integer) -> Int -> m Integer
>     fibMG fm 0 = return 1
>     fibMG fm 1 = return 1
>     fibMG fm n = liftM2 (+) (fm (n - 1)) (fm (n - 2))
> 
>     memoM :: (Show k, Show v, M4.MapLike c k v, MonadState c m) =>
>      ((k -> m v) -> k -> m v) -> (k -> m v) -> k -> m v
>     memoM f' f n = do
>       table <- get
>       case M4.lookup n table of
>         Just y -> return y
>         Nothing -> do
>           v <- f' f n
>           table' <- get
>           put $ M4.add n v table'
>           return v

`fibA`는 `Array`와 lazy-evaluation 사용

> fibA :: Int -> Integer
> fibA n0 = memo ! n0
>   where memo = listArray (0, n0) [f i | i <- [0..n0]]
>         f 0 = 1
>         f 1 = 1
>         f n = memo ! (n - 1) + memo ! (n - 2)

`fibLz`는 `List`를 사용한 lazy evalutation

> fibLz :: Int -> Integer
> fibLz = (series !!)
>   where series = 1:1:zipWith (+) series (tail series)

`fibIt`는 tail-recursive iteration 구현

> fibIt :: Int -> Integer
> fibIt n0 | n0 < 2 = 1
>          | otherwise = go 2 1 1
>   where go n a b | n == n0 = a + b
>                  | otherwise = go (n + 1) (a + b) a

Fujitsu P1620 랩탑에서 100000번째 fibonacci 수를 구하는데 걸리는 시간을 재봤다.

> main = do
>   n <- liftM (read. head) getArgs
>   vs <- mapM (time . ($n)) [
>     M1.memoize fib,   -- worst, same performance as fib
>     M1.memoFix fibG, -- memoize
>     fibMC, -- data-memocombinators
>     fix (M3.memo . fibG), --MemoTrie
>     fibMM I.empty, fibMM H.empty, fibMM M.empty, -- monad-memo
>     fibST I.empty, fibST H.empty, fibST M.empty, -- state monad
>     fibA, -- lazy evaluation with Array
>     fibIt, -- tail-recursive iteration
>     fibLz,
>     fibCM M.empty -- custom monad memo
>     ]
>   print $ all (== head vs) $ tail vs
> 
> time m = do
>   start <- getCPUTime
>   v <- evaluate m
>   end <- getCPUTime
>   printf "%0.9f sec \n" ((fromIntegral (end - start) :: Double) / 10^12)
>   return v

`M1.memoize fib`는 거의 사용 불가, `fib`와 거의 같은 성능을 보인다.

`M1.memofix fibG`는 3.432s

`fibMC`는 6.605s

`fix (M3.memo . fibG)`는 4.764s

`fibMM`는 1.252s, 1.460s, 1.492s

`fibST`는 1.620s, 2.012s, 2.304s

`fibCM`은 1.727s, 다만, HashMap 이나 Map 사용시 stack overflow가 발생한다.

`fibA`는 1.220s

`fibLz`는 1.216s.

`fibIt`는 0.252s,

memoization package 순위

1. 1.252s, monad-memo
1. 3.432s, memoize
1. 4.764s, memo-trie
1. 6.605s, data-memocombinators

기타 순위

1. 0.252s, iteration
1. 1.216s, lazy-evaluation
1. 1.220s, lazy-evaluation using Array
1. 1.620s, state monad
1. 1.727s, custom monad memo

그나마, monad-memo가 memoization package 중에서 가장 좋은 성능을 보인다.