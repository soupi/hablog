{-# LANGUAGE OverloadedStrings #-}

module Hablog.Run where

import           Web.Scotty
import           Web.Scotty.TLS (scottyTLS)
import           Control.Monad (liftM)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Text.Blaze.Html.Renderer.Text as HR
import qualified Network.Mime as Mime (defaultMimeLookup)

import Hablog.Settings
import Hablog.Present
import Hablog.Html (errorPage)

run :: Int -> IO ()
run port = scotty port router

runTLS :: Int -> FilePath -> FilePath -> IO ()
runTLS port key cert = scottyTLS port key cert router

router :: ScottyM ()
router = do
  get "/" presentMain
  get "/post/:yyyy/:mm/:dd/:title" $ do
    yyyy <- param "yyyy"
    mm <- param "mm"
    dd <- param "dd"
    title <- param "title"
    presentPost (mconcat [yyyy,"-",mm,"-",dd]) title
  get (regex "/static/(.*)") $ do
    path <- liftM (drop 1 . T.unpack) (param "0")
    if hasdots path then
      fail "no dots in path allowed"
      else do
        let mime = Mime.defaultMimeLookup (T.pack path)
        setHeader "content-type" $ TL.fromStrict (T.decodeUtf8 mime)
        file path
  get "/tags"
    presentTags
  get "/tags/:tag" $ do
    tag <- param "tag"
    presentTag tag
  get "/authors"
    presentAuthors
  get "/authors/:author" $ do
    author <- param "author"
    presentAuthor author
  get "/page/:page" $ do
    page <- param "page"
    presentPage page
  notFound $ html $ HR.renderHtml $ errorPage (blogTitle `TL.append` " - 404: not found") "404 - Could not find the page you were looking for."

hasdots :: String -> Bool
hasdots [] = False
hasdots ('.':'.':_) = True
hasdots (_:rest) = hasdots rest
