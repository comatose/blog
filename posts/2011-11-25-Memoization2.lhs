---
title: Memoization 구현 2
author: comatose
tags: haskell, memoization
---
[아래](http://comatose.github.com/blog/_site/posts/2011-11-25-Memoization.html)와 관련해서 다음과 같은 구현도 가능하다. 출처. [여기](http://www.kennknowles.com/blog/2008/05/07/ctl-model-checking-in-haskell-a-classic-algorithm-explained-as-memoization/)

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
> memo' :: (MonadState (M.Map k v) m, Ord k) =>
>          (k -> m v) -> k -> m v
> memo' f' n = do
>   table <- get
>   case M.lookup n table of
>     Just y -> return y
>     Nothing -> do
>       v <- f' n
>       modify $ M.insert n v
>       return v
> 
> fibMemo' = (`evalState` M.empty) . (fix (fibM' . memo'))
> fibMemo'' = (`evalState` M.empty) . (fix (memo' . fibM'))

`fibMemo`는 `fibM'`을 wrap한 것이고 `fibMemo'`과 `fibMemo''`는 mixin 기법을 쓴 것이다.

`fix`의 정의를 이용해서 `fix (fibM' . memo')`을 전개해보면

`fibM' . memo' . fibM' . memo' ...` 인 것을 알 수 있다.

mixin이란 용어는 여기서 유래한 것이 아닌가 싶다.