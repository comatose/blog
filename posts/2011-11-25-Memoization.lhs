---
title: Memoization 구현
author: comatose
tags: haskell, memoization
---

> {-# LANGUAGE NoMonomorphismRestriction #-}
> {-# LANGUAGE FlexibleContexts #-}
> 
> import Control.Monad.Fix (fix)
> import qualified Data.Map as M
> import Control.Monad.State hiding (fix)
> 
> fibM' :: (Monad m) => (Int -> m Integer) -> Int -> m Integer
> fibM' _ 0 = return 0
> fibM' _ 1 = return 1
> fibM' fibM n = liftM2 (+) (fibM (n - 2)) (fibM (n - 1))
>   
> memo :: (MonadState (M.Map k v) m, Ord k) =>
>          ((k -> m v) -> k -> m v) -> (k -> m v) -> k -> m v
> memo f' f n = do
>   table <- get
>   case M.lookup n table of
>     Just y -> return y
>     Nothing -> do
>       v <- f' f n
>       modify $ M.insert n v
>       return v
> 
> fibMemo = (`evalState` M.empty) . (fix (memo fibM'))

나름 `State` 모나드와 `Map`을 이용해서 memoization을 구현했다.

작동은 잘 되지만, `fibM'`처럼 기존 `fib`를 Monad로 재정의 해야한다는 것은 아쉽다.

아래 소개된 MemoTrie는 과연 어떻게 한건지...