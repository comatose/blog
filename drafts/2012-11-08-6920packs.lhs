---
title: Available in 6-packs, 9-packs, 20-packs
author: comatose
tags: dynamic programming, haskell
---

[출처.](http://www.haskell.org/haskellwiki/Dynamic_programming_example)

< A fast food place sells a finger food in only boxes of 6 pieces, boxes of 9 pieces, or boxes of 20 pieces. You can only buy zero or more such boxes. Therefore it is impossible to buy exactly 5 pieces, or exactly 7 pieces, etc. Can you buy exactly N pieces?

> import Control.Monad
> import Data.Array
> import System.Environment
> import Debug.Hood.Observe

> naiveSolve :: Int -> Maybe (Int, Int, Int)
> naiveSolve n | n < 0 = Nothing
>              | n == 0 = Just (0, 0, 0)
>              | otherwise =
>                do (x, y, z) <- naiveSolve (n - 20)
>                   return (x, y, z + 1)
>                `mplus`
>                do (x, y, z) <- naiveSolve (n - 9)
>                   return (x, y + 1, z)
>                `mplus`
>                do (x, y, z) <- naiveSolve (n - 6)
>                   return (x + 1, y, z)

> memoSolve :: Int -> Maybe (Int, Int, Int)
> memoSolve n0 = valOf n0
>   where
>     table = listArray (1, n0) . map f $ [1..n0]
>     valOf' n | n < 0 = Nothing
>             | n == 0 = Just (0, 0, 0)
>             | otherwise = table ! n
>     valOf = observe "valOf" valOf'
>     f n = do (x, y, z) <- valOf (n - 20)
>              return (x, y, z + 1)
>           `mplus`
>           do (x, y, z) <- valOf (n - 9)
>              return (x, y + 1, z)
>           `mplus`
>           do (x, y, z) <- valOf (n - 6)
>              return (x + 1, y, z)

> main :: IO ()
> main = getArgs >>= print . naiveSolve . read . head
