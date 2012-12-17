---
title: Monad Memo
author: comatose
tags: haskell, memoization
---

[Memoization Benchmark](2012-11-02-MemoizationRace.html)에서 비교적 좋은 성능을 보였던 monad-memo의 [source](http://hackage.haskell.org/packages/archive/monad-memo/0.3.0/doc/html/src/Control-Monad-Memo-Class.html#memo)를 살펴 보니,

< memoln :: (MonadCache k2 v m1, Monad m1, Monad m2) =>
<            (forall a.m1 a -> m2 a) -> (k1 -> k2)  -> (k1 -> m2 v) -> k1 -> m2 v
< memoln fl fk f k = do
<  mr <- fl $ lookup (fk k)
<  case mr of
<    Just r -> return r
<    Nothing -> do
<                r <- f k
<                fl $ add (fk k) r
<                return r

여기 `memoln`이 memoization의 핵심 함수인데, [여기](2011-11-25-Memoization.html)에 구현한 `memo`를 조금 더 일반화한 형태였다.

> {-# LANGUAGE FlexibleContexts      #-}
> {-# LANGUAGE FlexibleInstances     #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
>
> module Main where
>
> import           Control.Monad
> import           Control.Monad.State
> import qualified Data.IntMap           as I
> import qualified Control.Monad.Memo    as M4
> import           System.Environment

`fibMM`이 monad-memo를 이용한 것

> fibMM :: (M4.MapLike c Int Integer) => c -> Int -> Integer
> fibMM ml = (`M4.evalMemoState` ml) . f
>   where f 0 = return 1
>         f 1 = return 1
>         f n = liftM2 (+) (M4.memo f (n - 1)) (M4.memo f (n - 2))

`fibMG`는 open recursion 형태

> fibMG :: Monad m => (Int -> m Integer) -> Int -> m Integer
> fibMG _ 0 = return 1
> fibMG _ 1 = return 1
> fibMG f n = liftM2 (+) (f (n - 1)) (f (n - 2))

`fibCM`은 state를 이용한 hand-written 형태

> fibCM :: (M4.MapLike c Int Integer) => c -> Int -> Integer
> fibCM ml = (`evalState` ml) . (fix (memo . fibMG))
>   where
>     memo :: (M4.MapLike c k v, MonadState c m) =>
>             (k -> m v) -> k -> m v
>     memo f n = do
>       table <- get
>       case M4.lookup n table of
>         Just y -> return y
>         Nothing -> do
>           v <- f n
>           modify $ M4.add n v
>           return v

`fibMM'`은 fix와 open recursion을 활용한 monad-memo

> fibMM' :: (M4.MapLike c Int Integer) => c -> Int -> Integer
> fibMM' ml = (`M4.evalMemoState` ml) . (fix (M4.memo . fibMG))

> main :: IO ()
> main = getArgs >>= print . (fibMM' I.empty) . read . head

그런데, 실제 성능은 `fibMM`이 조금 더 좋고, `fibCM`은 stack overflow가 발생하는데 이유는 아직..