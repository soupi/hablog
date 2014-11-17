title: Haskell syntax highlight test
authors: Gil
tags: markdown, syntax, haskell, programming

Lets write some Haskell code
=============================

Alright:

```haskell
module Main where

import Data.List (concatMap, toUpper)

main :: IO ()
main = putStrLn $ concatMap toUpper ["Hello", "World"]

```

now let's see how it went
