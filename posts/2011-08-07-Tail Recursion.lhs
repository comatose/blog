---
title: Tail Recursion 만들기
author: comatose
tags: haskell, recursion
---

출처. ["SICP is Under Attack"](http://vedantk.tumblr.com/post/8424437797/sicp-is-under-attack-updated)

미국 유명대학교에서 프로그래밍 언어 교재로 쓰이던 SICP가 점점 사라지는 추세에 대한 의견.
본문 중 예제로 사용된 "용잡기" (To slay a dragon) 부분이 유용한 것 같아 Haskell로 번역하여 남김.

> module Main where

> f :: Integer -> Integer
> f n | n < 3 = n
>     | otherwise = f (n - 1) + 2 * f (n - 2) + 3 * f (n - 3)

`f` 를 tail-recursive `f'`으로 바꾸는 과정은 다음과 같다.

> f' :: Integer -> Integer
> f' n0 | n0 < 3 = n0
>       | otherwise = go 3 2 2 0

일단 3보다 작은 경우는 `f`와 동일하며, 그렇지 않은 경우는 tail-recursive한 `go`를 호출한다.

>   where
>     go :: Integer -> Integer -> Integer -> Integer -> Integer
>     go a b c n
>       | n == n0 = (a + b + c)
>       | otherwise =
>         let a' = a + b + c
>             b' = 2 * a
>             c' = b * 3 `div` 2
>         in go a' b' c' (n + 1)

`a, b, c`는 전달되는 state 이며, 아래 처럼 생각하면 된다.

< a = f (n - 1)
< b = 2 * f (n - 2)
< c = 3 * f (n - 3)

다음 단계로 진행될 때, `a, b, c`가 `a', b', c'`으로 변경되는 것을 풀어보면,

< f (n + 1) = f n + 2 * f (n - 1) + 3 * f (n - 2)

이고, `a, b, c`의 정의에 따라,

< f n = a + b + c = a'
< 2 * f (n - 1) = 2 * a = b'
< 3 * f (n - 2) = b * 3 `div` 2 = c'

위 코드의 let 정의와 일치한다.