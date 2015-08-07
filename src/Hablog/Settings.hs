{-# LANGUAGE OverloadedStrings #-}

module Hablog.Settings where

import Data.Text.Lazy (Text)
import Text.Blaze.Internal (AttributeValue)

blogTitle :: Text
blogTitle = "λm"

blogTheme :: Theme
blogTheme = lightTheme


blogPort :: Int
blogPort = 80

data Theme = Theme { bgTheme :: AttributeValue, codeTheme :: AttributeValue }

darkTheme :: Theme
darkTheme = Theme "static/css/dark.css" "static/highlight/styles/hybrid.css"

lightTheme :: Theme
lightTheme  = Theme "static/css/light.css" "static/highlight/styles/docco.css"
