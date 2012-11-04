---
title: Haskel Fixed Point Combinator 3
author: comatose
tags: haskell, fixed point combinator, Y combinator
---
n번째 fibonacci 수를 구하는 함수의 비효율적이지만 가장 유명한 구현 :

> fib 0 = 1
> fib 1 = 1
> fib n = fib (n - 1) + fib (n - 2)

역시 `fix`를 이용하여 구현 가능하다.

> fib fib 0 = 1
> fib fib 1 = 1
> fib fib n = fib (n - 1) + fib (n - 2)
> 
> fibSlow = fix fib

함수명 `fib`는 shadowing 되므로 인자 변수명 `fib`와는 별개임을 주의.
Haskell Syntax의 장점이 돋보인다.

지금도 충분히 흥미로운데 더욱 흥미로운 것은 이를 간단하게 Memoization을 할 수 있다.

> import Data.MemoTrie(memo)
> 
> fibFast :: Integer -> Integer
> fibFast = fix (memo . fib)
