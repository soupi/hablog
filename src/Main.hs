module Main where

import Run (run)
import Settings (blogPort)

main :: IO ()
main = run blogPort
