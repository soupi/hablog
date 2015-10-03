{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Concurrent (forkIO)
import System.Environment (getArgs)
import Hablog.Run (run, runTLS)
import Hablog.Settings (blogPort)

main :: IO ()
main =
  getArgs >>= \case
    [] ->
      run blogPort

    ["both", portStr, tlsPortStr, key, cert] ->
      case (reads portStr, reads tlsPortStr) of
        ([(port, "")], [(tlsPort, "")]) -> do
            _ <- forkIO $ run port
            runTLS tlsPort key cert
        _ ->
            putStrLn usageMsgBoth
    ["tls", portStr, key, cert] ->
      case reads portStr of
        [(port, "")] ->
          runTLS port key cert
        _ ->
          putStrLn usageMsgTLS
    [portStr] ->
      case reads portStr of
        [(port, "")] ->
          run port
        _ ->
          putStrLn usageMsg
    _ ->
      putStrLn usageMsg

usageMsg :: String
usageMsg =
  unlines
    ["Usage: hablog <port>"
    ,"or"
    ,usageMsgTLS
    ,"or"
    ,usageMsgBoth
    ]

usageMsgTLS :: String
usageMsgTLS = "Usage: hablog tls <port> <key file> <cert file>"

usageMsgBoth :: String
usageMsgBoth = "Usage: hablog both <port> <port tls> <key file> <cert file>"

