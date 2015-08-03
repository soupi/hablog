module Main where

import Hablog.Run (run)
import Hablog.Settings (blogPort)

main :: IO ()
main = run blogPort
