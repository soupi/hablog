{-# LANGUAGE OverloadedStrings #-}

module Settings where

import Data.Text.Lazy (Text)
import Text.Blaze.Internal (AttributeValue)

blogTitle :: Text
blogTitle = "Hablog"

blogTheme :: Theme
blogTheme = darkTheme


blogPort :: Int
blogPort = 8080

data Theme = Theme { bgTheme :: AttributeValue, codeTheme :: AttributeValue }

darkTheme :: Theme
darkTheme = Theme "static/css/dark.css" "static/highlight/styles/obsidian.css"

lightTheme :: Theme
lightTheme  = Theme "static/css/light.css" "static/highlight/styles/solarized_light.css"
