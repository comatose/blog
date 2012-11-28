---
title: MonadTrie
author: comatose
tags: haskell, memoization
---

> import Data.Array
> import Data.Function
> import qualified Data.MemoTrie as M
> import           System.Environment

[여기](2011-10-09-Y-Combinator3.html)에서 소개한 [MemoTrie]는 처음 접했을 때는 그냥 magic이었다.

> fibG :: (Int -> Integer) -> Int -> Integer
> fibG _ 0 = 1
> fibG _ 1 = 1
> fibG f n = f (n - 1) + f (n - 2)

> fibMagic = fix (M.memo . fibG)

IO를 쓰지 않고 라이브러리 수준에서 이렇게 간결한 memoization을 제공하는 게 가능한지 의심될 정도였는데, 최근에 [source](http://hackage.haskell.org/packages/archive/MemoTrie/0.5/doc/html/src/Data-MemoTrie.html#HasTrie)를 보고서 수수께끼를 풀었다.

결국, MemoTrie도 lazy evaluation을 이용한 memoization 인데, 예를 들면

> fibLazy :: Int -> Integer
> fibLazy = (tab !!)
>   where tab = map f [0..]
>         f 0 = 1
>         f 1 = 1
>         f n = tab !! (n - 1) + tab !! (n - 2)

일반적인 List를 이용한 memoization의 구현인 `fibLazy`에서 `f`를 open recursion으로 변형하고

> fibLazy' = (tab !!)
>   where tab = map (fibG (tab !!)) [0..]
>
> fibLazy'' = at
>   where at = (map (fibG at) [0..] !!)
>
> fibLazy''' = at
>   where at = (\f -> (map f [0..] !!)) (fibG at)
>
> fibLazy'''' = fix atG
>   where atG at = (\f -> (map f [0..] !!)) (fibG at)
>
> fibLazy''''' = fix atG
>   where atG = (\f -> (map f [0..] !!)) . fibG

이제, `memo`를 추출할 수 있다.

> fibLz :: (Int -> Integer)
> fibLz = fix (memo . fibG)
>   where memo :: (Int -> Integer) -> (Int -> Integer)
>         memo = \f -> (map f [0..] !!)

다시, `memo`를 좀 더 분리해보면,

> memo :: (Int -> Integer) -> (Int -> Integer)
> memo = untabify . tabify
>   where
>     tabify :: (Int -> Integer) -> [Integer]
>     tabify f = (map f [0..])
>     untabify :: [Integer] -> (Int -> Integer)
>     untabify = (!!)

결국 `memo`가 하는 일은 함수를 자료구조로 만들고(`tabify`) 다시 함수로 만들어주는(`untabify`) 것이다.

List가 비효율적이면 `memo'`처럼 Array를 사용할 수 있다.

> memo' :: Int -> (Int -> Integer) -> (Int -> Integer)
> memo' sz = untabify . tabify
>   where 
>     tabify :: (Int -> Integer) -> Array Int Integer
>     tabify f = listArray (0, sz) (map f [0..])
>     untabify :: Array Int Integer -> (Int -> Integer)
>     untabify = (!)

> fibLz' :: (Int -> Integer)
> fibLz' n = fix (memo' n . fibG) n

하지만, List나 Array는 양의 정수만 인자로 받기 때문에 좀 더 일반적인 container가 필요한데 [MemoTrie]는 이런 목적으로 trie를 사용한 것이다. 고로, `tabify` 는 `trie`로 `untabify`는 `untrie`가 된다.

> main :: IO ()
> main = getArgs >>= print . fibLz' . read . head

근데, [MonadMemo](2012-11-25-MonadMemo.html)가 더 빠르다.

[MemoTrie]:http://www.haskell.org/haskellwiki/MemoTrie
