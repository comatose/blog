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
> fib' :: (Num a) => (a -> a) -> a -> a
> fib' fib 0 = 0
> fib' fib 1 = 1
> fib' fib n = fib (n - 2) + fib (n - 1)
> 
> fib :: Num a => a -> a
> fib = fix fib'
> 
> fibM' :: (Num a, Monad m) => (a -> m a) -> a -> m a
> fibM' fibM 0 = return 0
> fibM' fibM 1 = return 1
> fibM' fibM n = do
>   x <- fibM (n - 2)
>   y <- fibM (n - 1)
>   return $ x + y
>   
> fibM :: (Num a, Monad m) => a -> m a
> fibM = fix fibM'
> 
> memo :: (MonadState (M.Map k v) m, Ord k) =>
>          ((k -> m v) -> k -> m v) -> (k -> m v) -> k -> m v
> memo f' f n = do
>   table <- get
>   case M.lookup n table of
>     Just y -> return y
>     Nothing -> do
>       v <- f' f n
>       put $ M.insert n v table
>       return v
> 
> fibMemo n = evalState (fix (memo fibM') n) M.empty

나름 State 모나드와 Map을 이용해서 memoization을 구현해 보았다.

fib'를 재활용하지 못 해 fibM'을 따로 만들어 줘야 하고 fibMemo의 정의도 지저분하다.

아래 소개된 MemoTrie는 과연 어떻게 한건지...