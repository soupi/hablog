-- | Hablog router

{-# LANGUAGE OverloadedStrings #-}

module Web.Hablog.Router where

import Web.Scotty.Trans
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Text.Blaze.Html.Renderer.Text as HR
import qualified Network.Mime as Mime (defaultMimeLookup)
import Network.URI (URI)
import Control.Monad
import Data.Maybe

import Web.Hablog.Types
import Web.Hablog.Present
import Web.Hablog.Html (errorPage)
import Web.Hablog.Post (eqY, eqYM, eqDate)

-- | Hablog's router
router :: Maybe URI -> Hablog ()
router domain = do
  get ("/favicon.ico") $ do
    let
      path = "static/favicon.ico"
      mime = Mime.defaultMimeLookup (T.pack path)
    setHeader "content-type" $ TL.fromStrict (T.decodeUtf8 mime)
    file path

  get (regex "/static/(.*)") $ do
    path <- fmap (drop 1 . T.unpack) (param "0")
    if hasdots path then
      fail "no dots in path allowed"
      else do
        let mime = Mime.defaultMimeLookup (T.pack path)
        setHeader "content-type" $ TL.fromStrict (T.decodeUtf8 mime)
        file path

  get "/" presentHome

  get "/blog" presentBlog

  blogRoute domain

  get "/:page" $ do
    page <- param "page"
    presentPage (TL.toLower page)

  notFound $ do
    cfg <- getCfg
    html $ HR.renderHtml $ errorPage cfg "404: not found" "404 - Could not find the page you were looking for."

  where
    hasdots [] = False
    hasdots ('.':'.':_) = True
    hasdots (_:rest) = hasdots rest


blogRoute :: Maybe URI -> Hablog ()
blogRoute domain = do
  when (isJust domain)
    $ get "/blog/atom.xml" presentAtom

  when (isJust domain)
    $ get "/blog/rss" presentRSS

  get "/blog/post/:yyyy/:mm/:dd/:title" $ do
    (yyyy, mm, dd) <- getDate
    title <- param "title"
    presentPost (mconcat [yyyy,"/",mm,"/",dd, "/", title])

  get "/blog/post/:yyyy/:mm/:dd" $ do
    (yyyy, mm, dd) <- getDate
    showPostsWhere (eqDate (yyyy, mm, dd))

  get "/blog/post/:yyyy/:mm" $ do
    yyyy <- param "yyyy"
    mm <- param "mm"
    showPostsWhere (eqYM (yyyy, mm))

  get "/blog/post/:yyyy" $ do
    yyyy <- param "yyyy"
    showPostsWhere (eqY yyyy)

  get "/blog/tags"
    presentTags

  get "/blog/tags/:tag" $ do
    tag <- param "tag"
    presentTag tag

  get "/blog/authors"
    presentAuthors

  get "/blog/authors/:author" $ do
    author <- param "author"
    presentAuthor author

  -- redirects

  when (isJust domain)
    $ get "/atom.xml" $ do
      redirect "/blog/atom.xml"

  when (isJust domain)
    $ get "/rss" $ do
      redirect "/blog/rss"

  get "/post/:yyyy/:mm/:dd/:title" $ do
    (yyyy, mm, dd) <- getDate
    title <- param "title"
    redirect $ mconcat ["/blog/post/", yyyy, "/", mm, "/", dd, "/", title]

  get "/post/:yyyy/:mm/:dd" $ do
    (yyyy, mm, dd) <- getDate
    redirect $ mconcat ["/blog/post/", yyyy, "/", mm, "/", dd]

  get "/post/:yyyy/:mm" $ do
    yyyy <- param "yyyy"
    mm <- param "mm"
    redirect $ mconcat ["/blog/post/", yyyy, "/", mm]

  get "/post/:yyyy" $ do
    yyyy <- param "yyyy"
    redirect $ mconcat ["/blog/post/", yyyy]

  get "/tags" $ do
    redirect "/blog/tags"

  get "/tags/:tag" $ do
    tag <- param "tag"
    redirect $ mconcat ["/blog/tags/", tag]

  get "/authors" $ do
    redirect "/blog/authors"

  get "/authors/:author" $ do
    author <- param "author"
    redirect $ mconcat ["/blog/authors/", author]

  where
    getDate =  (,,)
      <$> param "yyyy"
      <*> param "mm"
      <*> param "dd"
