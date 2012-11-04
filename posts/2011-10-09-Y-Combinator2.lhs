---
title: Haskel Fixed Point Combinator 2
author: comatose
tags: haskell, fixed point combinator, Y combinator
---
fix 함수의 구현은 다음과 같다.

> -- http://playingwithpointers.com/archives/590
> fix' f0 = applyInfinitely f0 undefined
>   where
>     applyInfinitely f x = f (applyInfinitely f x)
>
> -- http://hackage.haskell.org/packages/archive/base/latest/doc/html/src/Data-Function.html#fix
> -- | @'fix' f@ is the least fixed point of the function @f@,
> -- i.e. the least defined @x@ such that @f x = x@.
> fix :: (a -> a) -> a
> fix f = let x = f x in x

`fix'`은 `fix`를 좀 더 풀어쓴 형태다.
결과적으로 `fix`는 `f`를 `f(f(f(... )))` 형태로 무한히 적용한다.

`factorial`의 재귀 버전과 `fix` 버전은 다음과 같다.

> -- recursive factorial
> fact n = if n == 0 then 1 else n * fact (n-1)
> 
> -- factorial by fix
> fact = fix (\rec n -> if n == 0 then 1 else n * rec (n-1))

`fix`를 이용해서 anonymous recursion이 가능해진다.

`fix` 버전 `fact`를 다음과 같이 정의에 따라 확장해도 되고,

> fact' = \rec n -> if n == 0 then 1 else n * rec (n-1)
> 
> fix fact'
>   = let x = fact' x
>     in x
>   = fact' x
>   = (\rec n -> if n == 0 then 1 else n * rec (n-1)) x
>   = \n -> if n == 0 then 1 else n * x (n-1)
>   = \n -> if n == 0 then 1 else n * fact' x (n-1)
>   = \n -> if n == 0 then 1
>           else n * (\rec n' -> if n' == 0 then 1 else n' * rec (n'-1)) x (n-1)
>   = \n -> if n == 0 then 1
>           else n * (if n-1 == 0 then 1 else (n-1) * x (n-2))
>   = \n -> if n == 0 then 1
>           else n * (if n-1 == 0 then 1 else (n-1) * fact' x (n-2))
>   = \n -> if n == 0 then 1
>           else n * (if n-1 == 0 then 1
>                     else (n-1) * (if n-2 == 0 then 1
>                                   else (n-2) * fact' x (n-3)))
>   = ...

또는 `fix`의 denotational semantics에 의해 다음도 가능하다.

> -- fix will find a fixed point of fact', i.e. the function f such that f == fact' f.
> -- But let's expand what this means:
> 
> f = fact' f
>   = \n -> if n == 0 then 1 else n * f (n-1)

`fix fact' == fact` 가 확인된다.