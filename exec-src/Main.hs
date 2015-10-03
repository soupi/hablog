{-# LANGUAGE LambdaCase #-}

module Main where

import System.Environment (getArgs)
import Hablog.Run (run)
import Hablog.Settings (blogPort)

main :: IO ()
main = do
  getArgs >>= \case
    [] ->
      run blogPort
    [portStr] ->
      case reads portStr of
        [(port, "")] ->
          run port
        _ ->
          putStrLn usageMsg
    _ ->
      putStrLn usageMsg

usageMsg :: String
usageMsg = "Usage: hablog [<port>]"
