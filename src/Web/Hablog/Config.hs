-- | Configuration for Hablog

{-# LANGUAGE OverloadedStrings #-}

module Web.Hablog.Config where

import Data.Text.Lazy (Text)
import Text.Blaze.Internal (AttributeValue)

-- | Data type to set the theme for your Hablog blog
data Theme = Theme
  { bgTheme   :: AttributeValue -- ^ General theme for hablog. a file path for a css file
  , codeTheme :: AttributeValue -- ^ Theme for code. a file path for a highlight.js css file
  }

-- | Configuration for Hablog
data Config = Config
  { blogTitle :: Text
  , blogTheme :: Theme
  }

-- | Requires the needed values for runTLS
data TLSConfig = TLSConfig
  { blogTLSPort :: Int
  , blogCert    :: FilePath
  , blogKey     :: FilePath
  }

-- | A default configuration
defaultConfig :: Config
defaultConfig = Config
  { blogTitle = defaultTitle
  , blogTheme = defaultTheme
  }

-- | "Hablog"
defaultTitle :: Text
defaultTitle = "Hablog"

-- | The default HTTP port is 80
defaultPort :: Int
defaultPort = 80

-- | The default HTTPS port is 443
defaultTLSPort :: Int
defaultTLSPort = 443

-- | The default is the dark theme
defaultTheme :: Theme
defaultTheme = darkTheme

darkTheme :: Theme
darkTheme = Theme "/static/css/dark.css" "/static/highlight/styles/hybrid.css"

lightTheme :: Theme
lightTheme  = Theme "/static/css/light.css" "/static/highlight/styles/docco.css"

