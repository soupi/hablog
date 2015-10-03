title: Haskell syntax highlight test
authors: Gil
route: haskell-code-test
date: 2014-11-08
tags: markdown, syntax, haskell, programming
---

Lets write some Haskell code
=============================

Alright:

```haskell
module Main where

import Data.List (concatMap, toUpper)

main :: IO ()
main = putStrLn $ concatMap toUpper ["Hello ", "Hablog!"]

```

Voila!
