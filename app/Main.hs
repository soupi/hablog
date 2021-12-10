-- | A simple executable to run Hablog

{-# LANGUAGE LambdaCase #-}

module Main where

import Data.List (intercalate)
import Data.Text.Lazy (pack, unpack)
import Options.Applicative
import Web.Hablog

main :: IO ()
main = do
  cfg <- execParser configParserInfo
  run cfg

--------------------
-- Options Parser
--------------------

configParserInfo :: ParserInfo Config
configParserInfo =
  info (helper <*> config) $
     fullDesc
  <> header "Hablog - A blogging System"

config :: Parser Config
config = Config
  <$> fmap pack ttl
  <*> fmap snd thm
  <*> fmap pack domain
  <*> prt
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

prt :: Parser Int
prt =
  option auto
  (long "port"
   <> short 'p'
   <> metavar "PORT"
   <> help "Port for HTTP"
   <> showDefault
   <> value 80
  )

fromFile :: Parser FilePath
fromFile =
  strOption
  (long "file"
   <> short 'f'
   <> metavar "FILE"
   <> help "Path to configuration file"
  )
