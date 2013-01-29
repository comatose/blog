---
title: Algebraic Data Types
author: comatose
tags: haskell, ADT, continuation
---

[출처](http://www.haskellforall.com/2012/12/the-continuation-monad.html)

Type algebra says that if we squint then we can translate the following type constructors to algebraic operators and derive equivalent types from simple algebraic manipulations:

> Either a b  <=>  a + b
> (a, b)      <=>  a * b
> a -> b      <=>  b ^ a

That means that if we have a function with two continuations:

> (a1 -> r) -> ((a2 -> r) -> r)

... we just translate it to the equivalent algebraic expression:

> (r ^ (r ^ a2)) ^ (r ^ a1)

... and then we can derive equivalent representations just by using the rules of algebra:

>   (r ^ (r ^ a2)) ^ (r ^ a1)
> = r ^ ((r ^ a2) * (r ^ a1))
> = r ^ (r ^ (a2 + a1))

... then if we translate that back to the equivalent type, we get:

> (Either a2 a1 -> r) -> r

... which is exactly the trick described in the previous section.

Similarly, if we have more than one argument to a continuation:

> (a -> b -> r) -> r

... we can find an equivalent single-argument form using type algebra:

>   r ^ ((r ^ a) ^ b)
> = r ^ (r ^ (a * b))

... which transforms back to:

> ((a, b) -> r) -> r

So type algebra tells us the obvious: uncurry the continuation if it needs a single argument.
