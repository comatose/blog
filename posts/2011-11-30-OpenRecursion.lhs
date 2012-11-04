---
title: Open Recursion
author: comatose
tags: haskell, open recursion
---

> fib 0 = 1
> fib 1 = 1
> fib n = fib (n - 1) + fib (n - 2)
> 
> fibG self 0 = 1
> fibG self 1 = 1
> fibG self n = self (n - 1) + self (n - 2)
> 
> fib' = fix fibG

`fib'`과 같이 함수 자신의 이름을 body에서 참조하지 않고 구현하는 것을 Anonymous Recursion이라고 부르기도 하지만, 간혹 Open Recursion 이라고도 한다.

그 이유는 재귀적으로 호출될 함수를 나중에 결정(bind)할 수 있기 때문인데, `fib`를 살펴 보면, `fib` 내부에서 재귀적으로 호출될 함수를 `fib`로 고정(closed)시켰기 때문에 `fib`의 수정없이 동작을 바꾸기 어렵다.

하지만 `fib'`은 `fibG`를 수정하지 않고 `fib'`을 수정할 수 있다. (예, wrapping, memoization, extension 등)

즉, Open Recursion으로 노출된 라이브러리는 원본의 수정없이 쉽게 확장하거나 변경할 수 있다는 장점이 있다.

관련하여 흥미로운 예제로 [interpreter][1]의 구현이 있다. (De-serializing 부분 참고)

[1]:http://okmij.org/ftp/tagless-final/course/#infin1

### 참고 포스트 ###

* [http://lambda-the-ultimate.org/node/3204](http://lambda-the-ultimate.org/node/3204)

* [http://www.kennknowles.com/blog/2008/05/07/ctl-model-checking-in-haskell-a-classic-algorithm-explained-as-memoization/](http://www.kennknowles.com/blog/2008/05/07/ctl-model-checking-in-haskell-a-classic-algorithm-explained-as-memoization/)

* [http://okmij.org/ftp/tagless-final/course/SerializeExt.hs](http://okmij.org/ftp/tagless-final/course/SerializeExt.hs)