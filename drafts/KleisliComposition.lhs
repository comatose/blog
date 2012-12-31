---
title: Kleisli Composition
author: comatose
tags: haskell, memoization
---

> (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
> f >=> g = \x -> f x >>= g

>  mx >>= f = (id >=> f) mx
