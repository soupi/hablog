-- | A simple executable to run Hablog

{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad (void)
import Control.Concurrent (forkIO)
import System.Environment (getArgs)
import Web.Hablog

main :: IO ()
main =
  getArgs >>= \case
    [] ->
      run defaultConfig defaultPort

    ["both", portStr, tlsPortStr, key, cert] ->
      case (reads portStr, reads tlsPortStr) of
        ([(port, "")], [(tlsPort, "")]) -> do
            void $ forkIO $ run defaultConfig port
            runTLS
              TLSConfig { blogTLSPort = tlsPort, blogKey = key, blogCert = cert }
              defaultConfig
        _ ->
            putStrLn usageMsgBoth
    ["tls", portStr, key, cert] ->
      case reads portStr of
        [(port, "")] ->
            runTLS
              TLSConfig { blogTLSPort = port, blogKey = key, blogCert = cert }
              defaultConfig
        _ ->
          putStrLn usageMsgTLS

    [themeStr]
      | Just theme <- lookup themeStr themes ->
        run defaultConfig { blogTheme = theme } defaultPort

    [portStr]
      | [(port, "")] <- reads portStr ->
        run defaultConfig port

    [themeStr,portStr]
      | Just theme <- lookup themeStr themes
      , [(port, "")] <- reads portStr ->
        run defaultConfig { blogTheme = theme } port

    _ ->
      putStrLn usageMsg

usageMsg :: String
usageMsg =
  unlines
    [ "Usage: hablog <port>"
    , "or"
    , usageMsgTLS
    , "or"
    , usageMsgBoth
    ]

usageMsgTLS :: String
usageMsgTLS = "Usage: hablog tls <port> <key file> <cert file>"

usageMsgBoth :: String
usageMsgBoth = "Usage: hablog both <port> <port tls> <key file> <cert file>"

