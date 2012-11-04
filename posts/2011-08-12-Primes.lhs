---
title: lazy evaluation으로 소수열 구하기
author: comatose
tags: haskell, primes
---

당신이 사용하는 언어가 당신의 생각을 바꾼다.

> primes = sieve [2..]
>   where sieve (x:xs) = x : sieve [n | n <- xs, (n `rem` x) /= 0]

