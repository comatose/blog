---
title: Y in Haskell?
author: comatose
tags: haskell, fixed point combinator
---

untyped lambda calculus에서 Y는 다음과 같다.

< Y = λf.(λx.f (x x)) (λx.f (x x))

이걸 Haskell로 직역하면 아래와 같을텐데,

< y0 f = (\x -> f (x x)) (\x -> f (x x))  -- this is the pseudo-Haskell code

y0는 컴파일 되지 않는다. 왜냐하면 Haskell은 "typed" lambda calculus이기 때문이다.

`x`의 타입이 `a`라고 하면, `x x`를 가능하게 하려면 `x`의 타입은 `a -> a`가 되어야 한다. 이게 반복되면 `x`의 타입은 `a -> a -> .. -> a`가 되는데 Haskell은 무한한 타입을 허용하지 않는다.

대신 아래처럼 Contructor로 wrapping하면 가능하다.

> newtype SelfApply t = SelfApply { selfApply :: SelfApply t -> t }
> 
> y :: (t -> t) -> t
> y f = selfApply term term
>   where term = (SelfApply $ \x -> f (selfApply x x))

* [출처](https://groups.google.com/forum/?fromgroups=#!topic/stanford-11au-cs240h/gRjdqUjD8_I) "Y combinator in Haskell"