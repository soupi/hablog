-- | A simple executable to run Hablog

{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad (void)
import Control.Concurrent (forkIO)
import Data.Monoid ((<>))
import Data.List (intercalate)
import Data.Text.Lazy (pack, unpack)
import Options.Applicative
import Web.Hablog

main :: IO ()
main = do
  args <- execParser paramsParserInfo
  let cfg = pCfg args
  case pCmd args of
    HTTP port ->
      run cfg port
    HTTPS tlsCfg ->
      runTLS tlsCfg cfg
    Both port tlsCfg -> do
      void $ forkIO $ run cfg port
      runTLS tlsCfg cfg


--------------------
-- Options Parser
--------------------

data Params = Params
  { pCfg :: Config
  , pCmd :: Command
  }
  deriving (Show, Read)

data Command
  = HTTP Int
  | HTTPS TLSConfig
  | Both Int TLSConfig
  deriving (Show, Read)

paramsParserInfo :: ParserInfo Params
paramsParserInfo =
  info (helper <*> (Params <$> config <*> cmd)) $
     fullDesc
  <> header   "Hablog - A blogging System"

config :: Parser Config
config = Config
  <$> fmap pack ttl
  <*> fmap snd thm
  <*> fmap pack domain
  where
    ttl =
      strOption
        (long "title"
         <> short 't'
         <> metavar "NAME"
         <> help "Title for the blog"
         <> showDefault
         <> value (unpack defaultTitle)
        )
    domain =
      strOption
        (long "domain"
         <> short 'd'
         <> metavar "NAME"
         <> help "Website domain"
         <> showDefault
         <> value (unpack defaultDomain)
        )


thm :: Parser (String, Theme)
thm =
  option (str >>= readTheme)
  (long "theme"
   <> short 'T'
   <> metavar "THEME"
   <> help "Select a blog theme"
   <> showDefaultWith fst
   <> value defaultTheme
  )

readTheme :: String -> ReadM (String, Theme)
readTheme themeStr =
  case lookup themeStr themes of
    Just tm -> pure (themeStr, tm)
    Nothing ->
      readerError $
        "'" ++ themeStr ++ "' is not a valid theme. Try one of: "
            ++ intercalate ", " (map fst themes)

cmd :: Parser Command
cmd =
  subparser
  ( command "http" (info (HTTP <$> httpConfig <**> helper)
      ( progDesc "Run only in HTTP mode" ))
 <> command "https" (info (HTTPS <$> tlsConfig <**> helper)
      ( progDesc "Run only in TLS mode" ))
 <> command "both" (info (Both <$> httpConfig <*> tlsConfig <**> helper)
      ( progDesc "Run both in HTTP and TLS modes" ))
  )

httpConfig :: Parser Int
httpConfig =
  option auto
  (long "port"
   <> short 'p'
   <> metavar "PORT"
   <> help "Port for HTTP"
   <> showDefault
   <> value 80
  )

tlsConfig :: Parser TLSConfig
tlsConfig = TLSConfig
  <$> option auto (long "tls-port" <> short 'P' <> metavar "PORT" <> help "Port for TLS" <> showDefault <> value 443)
  <*> strOption (long "tls-key"  <> short 'k' <> metavar "KEY"  <> help "Key file for for TLS")
  <*> strOption (long "tls-cert" <> short 'c' <> metavar "CERT" <> help "Cert file for for TLS")

fromFile :: Parser FilePath
fromFile =
  strOption
  (long "file"
   <> short 'f'
   <> metavar "FILE"
   <> help "Path to configuration file"
  )
