{-# language OverloadedStrings #-}

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
import qualified Data.Text.Encoding.Error as TL
import qualified Data.ByteString.Lazy as BSL

import qualified Text.Blaze.Html.Renderer.Text as HR
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html5 ((!))

import qualified System.Directory as Dir (getDirectoryContents, getModificationTime)
import System.IO.Error (catchIOError)

import Web.Hablog.Html
import Web.Hablog.Types
import Web.Hablog.Config
import Web.Hablog.Feed
import qualified Web.Hablog.Post as Post
import qualified Web.Hablog.Page as Page


presentHome :: HablogAction ()
presentHome = do
  allPages <- liftIO getAllPages
  cfg <- getCfg
  case L.find (\p -> Page.pageURL p == "home") allPages of
    Nothing -> presentBlog
    Just homePage -> do
      html $ HR.renderHtml $ template cfg (Page.pagePreview homePage) False "home" "home" $ do
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

presentRSS :: HablogAction ()
presentRSS = do
  cfg <- getCfg
  allPosts <- liftIO getAllPosts
  let mime = "application/rss+xml"
  setHeader "content-type" mime
  raw
    . TL.encodeUtf8
    . renderRSSFeed
    . rssFeed cfg
    $ allPosts

presentAtom :: HablogAction ()
presentAtom = do
  cfg <- getCfg
  allPosts <- liftIO getAllPosts
  let mime = "application/atom+xml"
  setHeader "content-type" mime
  raw
    . TL.encodeUtf8
    . renderAtomFeed
    . atomFeed cfg
    $ allPosts

showPostsWhere :: (Post.Post -> Bool) -> HablogAction ()
showPostsWhere test = do
  cfg <- getCfg
  allPosts <- liftIO getAllPosts
  html $ HR.renderHtml $ template cfg Post.noPreview False "Posts" "blog" $
    postsListHtml $ filter test allPosts

presentPage :: Text -> HablogAction ()
presentPage route = do
  pages <- liftIO getAllPages
  showOrNotFound (presentPage' pages) . filter (((==) $ TL.unpack $ TL.toLower route) . Page.pageURL) $ pages

presentPage' :: [Page.Page] -> Config -> Page.Page -> H.Html
presentPage' pages cfg page = do
  template cfg (Page.pagePreview page) False (Page.pageName page) (Page.pageURL page) $ do
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

getAllFromDir :: Ord a => (Post.UTCTime -> Text -> Maybe a) -> FilePath -> IO [a]
getAllFromDir parse dir = do
  posts <-
    fmap
      (L.delete ".." . L.delete ".")
      (Dir.getDirectoryContents dir `catchIOError` (\_ -> pure []))
  contents <-
    rights <$> mapM (readFileWithModTime . ((dir ++ "/") ++)) posts
  pure . L.sortBy (flip compare) . catMaybes $ fmap (uncurry parse) (reverse contents)

readFileWithModTime :: FilePath -> IO (Either TL.UnicodeException (Post.UTCTime, Text))
readFileWithModTime path =
  catchIOError
    ( do
      content <- TL.decodeUtf8' <$> BSL.readFile path
      modtime <- Dir.getModificationTime path
      pure $ fmap ((,) modtime) content
    )
    (const (pure $ Left undefined))

presentPost :: Text -> HablogAction ()
presentPost title = do
  posts <- liftIO getAllPosts
  showOrNotFound postPage $ filter ((== title) . path) posts
  where path p = TL.intercalate "/" ([Post.year, Post.month, Post.day, Post.postRoute] <*> [p])

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

getFromFile :: (Post.UTCTime -> Text -> Maybe a) -> String -> IO (Maybe a)
getFromFile constructor path = do
  result <- readFileWithModTime path
  let
    cont =
      case result of
        Left _  -> Nothing
        Right x -> Just x
  let content = uncurry constructor =<< cont
  pure content

getAllTags :: [Post.Post] -> [Text]
getAllTags = getAll Post.postTags

hasTag :: Text -> Post.Post -> Bool
hasTag tag = ([]/=) . filter (==tag) . Post.postTags

getAllAuthors :: [Post.Post] -> [Text]
getAllAuthors = getAll Post.postAuthors

getAll :: (Post.Post -> [Text]) -> [Post.Post] -> [Text]
getAll f  = L.sort . map (TL.unwords . TL.words . head) . L.group . L.sort . concatMap f

hasAuthor :: Text -> Post.Post -> Bool
hasAuthor auth myPost = auth `elem` Post.postAuthors myPost
