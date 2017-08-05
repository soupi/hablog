-- | Running Hablog

{-# LANGUAGE OverloadedStrings #-}

module Web.Hablog.Run where

import           Data.Monoid
import           Web.Scotty.Trans
import           Web.Scotty.TLS (scottyTTLS)
import           Control.Monad.Trans.Reader (runReaderT)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Text.Blaze.Html.Renderer.Text as HR
import qualified Network.Mime as Mime (defaultMimeLookup)
import Network.URI (URI, parseURI)
import Control.Monad
import Data.Maybe

import Web.Hablog.Types
import Web.Hablog.Config
import Web.Hablog.Present
import Web.Hablog.Html (errorPage)
import Web.Hablog.Post (eqY, eqYM, eqDate)


-- | Run Hablog on HTTP
run :: Config -> Int -> IO ()
run cfg port =
  scottyT port (`runReaderT` cfg') (router $! domain)
  where
    cfg' = cfg
      { blogDomain = "http://" <> blogDomain cfg <> ":" <> portStr }
    portStr = if port == 80 then "" else TL.pack (show port)
    domain = parseURI (TL.unpack $ blogDomain cfg')

-- | Run Hablog on HTTPS
runTLS :: TLSConfig -> Config -> IO ()
runTLS tlsCfg cfg =
  scottyTTLS (blogTLSPort tlsCfg) (blogKey tlsCfg) (blogCert tlsCfg) (`runReaderT` cfg') (router $! domain)
  where
    cfg' = cfg
      { blogDomain = "https://" <> blogDomain cfg <> ":" <> portStr }
    portStr = if blogTLSPort tlsCfg == 443 then "" else TL.pack (show (blogTLSPort tlsCfg))
    domain = parseURI (TL.unpack $ blogDomain cfg')

-- | Hablog's router
router :: Maybe URI -> Hablog ()
router domain = do
  get "/" presentMain
  when (isJust domain)
    $ get "/rss" (presentRSS $ fromJust domain)
  get "/post/:yyyy/:mm/:dd/:title" $ do
    (yyyy, mm, dd) <- getDate
    title <- param "title"
    presentPost (mconcat [yyyy,"/",mm,"/",dd, "/", title])
  get (regex "/static/(.*)") $ do
    path <- fmap (drop 1 . T.unpack) (param "0")
    if hasdots path then
      fail "no dots in path allowed"
      else do
        let mime = Mime.defaultMimeLookup (T.pack path)
        setHeader "content-type" $ TL.fromStrict (T.decodeUtf8 mime)
        file path
  get "/post/:yyyy/:mm/:dd" $ do
    (yyyy, mm, dd) <- getDate
    showPostsWhere (eqDate (yyyy, mm, dd))
  get "/post/:yyyy/:mm" $ do
    yyyy <- param "yyyy"
    mm <- param "mm"
    showPostsWhere (eqYM (yyyy, mm))
  get "/post/:yyyy" $ do
    yyyy <- param "yyyy"
    showPostsWhere (eqY yyyy)
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
  notFound $ do
    cfg <- getCfg
    html $ HR.renderHtml $ errorPage cfg (blogTitle cfg `TL.append` " - 404: not found") "404 - Could not find the page you were looking for."

  where
    getDate =  (,,)
           <$> param "yyyy"
           <*> param "mm"
           <*> param "dd"

    hasdots [] = False
    hasdots ('.':'.':_) = True
    hasdots (_:rest) = hasdots rest
