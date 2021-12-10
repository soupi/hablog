{-# LANGUAGE OverloadedStrings #-}

module Web.Hablog.Present
  ( module Web.Hablog.Present
  )
  where

import Web.Scotty.Trans
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (catMaybes)
import Data.Either (rights)
import qualified Data.List as L
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.ByteString.Lazy as BSL

import qualified Text.Blaze.Html.Renderer.Text as HR
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html5 ((!))

import qualified System.Directory as DIR (getDirectoryContents)
import System.IO.Error (catchIOError)

import Web.Hablog.Html
import Web.Hablog.Types
import Web.Hablog.Config
import qualified Web.Hablog.Post as Post
import qualified Web.Hablog.Page as Page


presentHome :: HablogAction ()
presentHome = do
  allPages <- liftIO getAllPages
  cfg <- getCfg
  case L.find (\p -> Page.getPageURL p == "home") allPages of
    Nothing -> presentBlog
    Just homePage -> do
      html $ HR.renderHtml $ template cfg (Page.getPagePreview homePage) False "home" "home" $ do
        H.nav ! A.class_ "menu" $ do
          H.ul ! A.class_ "pages" $ do
            pagesList allPages
            H.li $ H.a ! A.href "/blog" $ "Blog"
        H.div ! A.class_ "content" $ do
          pageContent homePage


presentBlog :: HablogAction ()
presentBlog = do
  allPosts <- liftIO getAllPosts
  allPages <- liftIO getAllPages
  tgs <- liftIO getTagList
  auths <- liftIO getAuthorsList
  cfg <- getCfg
  html $ HR.renderHtml $ template cfg Post.noPreview False "Blog" "blog" $ do
    H.nav ! A.class_ "menu" $ do
      H.ul ! A.class_ "pages" $ do
        pagesList allPages
        H.li $ H.a ! A.href "/blog" $ "Blog"
    H.div ! A.class_ "main-content" $ do
      postsListHtml allPosts
      H.aside ! A.class_ "aside" $ do
        H.div ! A.class_ "AllAuthorsList" $ do
          H.h1 "Authors"
          auths
        H.div ! A.class_ "AllTagsList" $ do
          H.h1 "Tags"
          tgs

{-
presentRSS :: URI -> HablogAction ()
presentRSS domain = do
  cfg <- getCfg
  allPosts <- liftIO getAllPosts
  let mime = "application/rss+xml"
  setHeader "content-type" mime
  raw
    . BSLC.pack
    . RSS.showXML
    . RSS.rssToXML
    . RSS.RSS (TL.unpack $ blogTitle cfg) domain "" []
    . map (Post.toRSS $ blogDomain cfg)
    $ allPosts
-}

showPostsWhere :: (Post.Post -> Bool) -> HablogAction ()
showPostsWhere test = do
  cfg <- getCfg
  allPosts <- liftIO getAllPosts
  html $ HR.renderHtml $ template cfg Post.noPreview False "Posts" "blog" $
    postsListHtml $ filter test allPosts

presentPage :: Text -> HablogAction ()
presentPage route = do
  pages <- liftIO getAllPages
  showOrNotFound (presentPage' pages) . filter (((==) $ TL.unpack $ TL.toLower route) . Page.getPageURL) $ pages

presentPage' :: [Page.Page] -> Config -> Page.Page -> H.Html
presentPage' pages cfg page = do
  template cfg (Page.getPagePreview page) False (Page.getPageName page) (Page.getPageURL page) $ do
    H.nav ! A.class_ "menu" $ do
      H.ul ! A.class_ "pages" $ do
        pagesList pages
        H.li $ H.a ! A.href "/blog" $ "Blog"
    H.div ! A.class_ "content" $ do
      pageContent page



getAllPages :: IO [Page.Page]
getAllPages = getAllFromDir Page.toPage "_pages"

getAllPosts :: IO [Post.Post]
getAllPosts = getAllFromDir Post.toPost "_posts"

getAllFromDir :: Ord a => (Text -> Maybe a) -> FilePath -> IO [a]
getAllFromDir parse dir = do
  posts <- fmap (L.delete ".." . L.delete ".") (DIR.getDirectoryContents dir `catchIOError` (\_ -> pure []))
  contents <- rights <$> mapM ((\x -> (TL.decodeUtf8' <$> BSL.readFile x) `catchIOError` const (pure $ Left undefined)) . ((dir++"/")++)) posts
  pure . L.sortBy (flip compare) . catMaybes $ fmap parse (reverse contents)

presentPost :: Text -> HablogAction ()
presentPost title = do
  posts <- liftIO getAllPosts
  showOrNotFound postPage $ filter ((== title) . path) posts
  where path p = TL.intercalate "/" ([Post.year, Post.month, Post.day, Post.route] <*> [p])

showOrNotFound :: (Config -> a -> H.Html) -> [a] -> HablogAction ()
showOrNotFound showP result = do
  cfg <- getCfg
  case result of
    (p:_) -> html $ HR.renderHtml $ showP cfg p
    []    -> html $ HR.renderHtml $ errorPage cfg "404: not found" "Could not find the page you were looking for."

presentTags :: HablogAction ()
presentTags = do
  cfg <- getCfg
  tags <- liftIO getTagList
  html . HR.renderHtml $ template cfg Post.noPreview False "Posts Tags" "blog" tags

getTagList :: IO H.Html
getTagList = pure . tagsList . getAllTags =<< getAllPosts

getPageList :: [Page.Page] -> H.Html
getPageList = H.ul . pagesList


getAuthorsList :: IO H.Html
getAuthorsList = pure . authorsList . getAllAuthors =<< getAllPosts

presentTag :: Text -> HablogAction ()
presentTag tag = do
  cfg <- getCfg
  posts <- liftIO getAllPosts
  html . HR.renderHtml . template cfg Post.noPreview False tag "blog" $ postsListHtml $ filter (hasTag tag) posts

presentAuthors :: HablogAction ()
presentAuthors = do
  cfg <- getCfg
  authors <- liftIO getAuthorsList
  html . HR.renderHtml $ template cfg Post.noPreview False "Posts Authors" "blog" authors

presentAuthor :: Text -> HablogAction ()
presentAuthor auth = do
  cfg <- getCfg
  posts <- liftIO getAllPosts
  html . HR.renderHtml . template cfg Post.noPreview False auth "blog" . postsListHtml $ filter (hasAuthor auth) posts

getPageFromFile :: Text -> IO (Maybe Page.Page)
getPageFromFile title = do
  let path = TL.unpack $ mconcat ["_pages/", title]
  getFromFile Page.toPage path

getPostFromFile :: Text -> Text -> IO (Maybe Post.Post)
getPostFromFile date title = do
  let postPath = TL.unpack $ mconcat ["_posts/", date, "-", title, ".md"]
  getFromFile Post.toPost postPath

getFromFile :: (Text -> Maybe a) -> String -> IO (Maybe a)
getFromFile constructor path = do
  fileContent <- (TL.decodeUtf8' <$> BSL.readFile path) `catchIOError` const (pure $ Left undefined)
  let cont = case fileContent of
              Left _  -> Nothing
              Right x -> Just x
  let content = constructor =<< cont
  pure content

getAllTags :: [Post.Post] -> [Text]
getAllTags = getAll Post.tags

hasTag :: Text -> Post.Post -> Bool
hasTag tag = ([]/=) . filter (==tag) . Post.tags

getAllAuthors :: [Post.Post] -> [Text]
getAllAuthors = getAll Post.authors

getAll :: (Post.Post -> [Text]) -> [Post.Post] -> [Text]
getAll f  = L.sort . map (TL.unwords . TL.words . head) . L.group . L.sort . concatMap f

hasAuthor :: Text -> Post.Post -> Bool
hasAuthor auth myPost = auth `elem` Post.authors myPost
