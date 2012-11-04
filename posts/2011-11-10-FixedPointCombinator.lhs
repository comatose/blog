---
title: Fixed Point Combinator Video by Heinrich Apfelmus
author: comatose
tags: haskell, fixed point combinator
---
[http://apfelmus.nfshost.com/blog/2010/07/02-fixed-points-video/player.html](http://apfelmus.nfshost.com/blog/2010/07/02-fixed-points-video/player.html)

12분 54초, Mind-Blowing에 충분한 시간

> -- 1) factorial with recursion
> fact = \n -> if n == 0 then 1 else n * fact (n - 1)
> 
> -- 2) fact = fact' fact
> fact' = \f n -> if n == 0 then 1 else n * f (n - 1)
> 
> -- 3) fact = fix fact'
> fix f' = let f = f' f in f
> fix' f' = f' (fix' f')

1)
`fact`를 인자로 나머지 루틴을 일종의 패턴으로 추출하여,
`fact = fact' fact` 를 만족시키도록 `fact'`을 새로 정의한다.

2)
`fact'`과 `fact`는 생김새도 비슷하며 서로 밀접하게 연관되어 있다.
(하지만, `fact`는 재귀 형식이지만, `fact'`은 재귀적이지 않다.)
이제, `fact'`에서 `fact`를 도출해주는 `fix` 함수를 정의한다.
(즉, `fact = fix fact'`)

3)
`fix'`을 보면 haskell의 `fix`는 재귀적임을 알 수 있다.
`fact`는 재귀적이다. `fact'`은 재귀적이지 않다. 다시 `fix`는 재귀적이다.
"The fixed point combinator is the essence of recursion." 에 공감하게 된다.

하지만, 놀랍게도 untyped lambda calculus의 `fix`는 재귀적이지 않다.

<    Y = λf.(λx.f (x x)) (λx.f (x x))