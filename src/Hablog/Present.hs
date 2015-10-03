{-# LANGUAGE OverloadedStrings #-}

module Hablog.Present where

import           Web.Scotty
import           Control.Monad (liftM)
import           Control.Monad.Trans.Class (lift)
import           Data.Maybe (catMaybes)
import           Data.Either (rights)
import qualified Data.List as L
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.ByteString.Lazy as BSL

import qualified Text.Blaze.Html.Renderer.Text as HR
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html5 ((!))

import qualified System.Directory as DIR (getDirectoryContents)
import           System.IO.Error (catchIOError)

import Hablog.Html
import qualified Hablog.Post as Post
import qualified Hablog.Page  as Page

presentMain :: ActionM ()
presentMain = do
  allPosts <- lift getAllPosts
  allPages <- lift getAllPages
  tgs <- lift getTagList
  auths <- lift getAuthorsList
  html $ HR.renderHtml $ template "Posts" $ do
    H.aside ! A.class_ "aside" $ do
      presentPagesList allPages
      H.div ! A.class_ "AllAuthorsList" $ do
        H.h1 "Authors"
        auths
      H.div ! A.class_ "AllTagsList" $ do
        H.h1 "Tags"
        tgs
    postsListHtml allPosts


presentPagesList :: [Page.Page] -> H.Html
presentPagesList [] = return ()
presentPagesList pages =
  H.div ! A.class_ "AllAuthorsList" $ do
    H.h1 "Pages"
    getPageList pages


presentPage :: T.Text -> ActionM ()
presentPage title = do
  pages  <- lift getAllPages
  case filter ((== T.unpack title) . Page.getPageURL) pages of
    (p:_) -> html $ HR.renderHtml $ pagePage p
    _     -> html $ HR.renderHtml $ errorPage "Hablog - 404: not found" "Could not find the page you were looking for."

getAllPages :: IO [Page.Page]
getAllPages = getAllFromDir Page.toPage "_pages"

getAllPosts :: IO [Post.Post]
getAllPosts = getAllFromDir Post.toPost "_posts"

getAllFromDir :: Ord a => (T.Text -> Maybe a) -> FilePath -> IO [a]
getAllFromDir parse dir = do
  posts <- liftM (L.delete ".." . L.delete ".") (DIR.getDirectoryContents dir `catchIOError` (\_ -> return []))
  contents <- rights <$> mapM ((\x -> (T.decodeUtf8' <$> BSL.readFile x) `catchIOError` const (pure $ Left undefined)) . ((dir++"/")++)) posts
  pure . L.sortBy (flip compare) . catMaybes $ fmap parse (reverse contents)



presentPost :: T.Text -> ActionM ()
presentPost title = do
  posts <- lift getAllPosts
  case filter ((== title) . path) posts of
    (p:_) -> html $ HR.renderHtml $ postPage p
    []    -> html $ HR.renderHtml $ errorPage "Hablog - 404: not found" "Could not find the page you were looking for."
  where path p = T.intercalate "/" ([Post.year, Post.month, Post.day, Post.route] <*> [p])

presentTags :: ActionM ()
presentTags = html . HR.renderHtml . template "Posts Tags" =<< lift getTagList

getTagList :: IO H.Html
getTagList = return . tagsList . getAllTags =<< getAllPosts

getPageList :: [Page.Page] -> H.Html
getPageList = pagesList


getAuthorsList :: IO H.Html
getAuthorsList = return . authorsList . getAllAuthors =<< getAllPosts

presentTag :: T.Text -> ActionM ()
presentTag tag = html . HR.renderHtml . template tag . postsListHtml . filter (hasTag tag) =<< lift getAllPosts

presentAuthors :: ActionM ()
presentAuthors = html . HR.renderHtml . template "Posts Authors" =<< lift getAuthorsList

presentAuthor :: T.Text -> ActionM ()
presentAuthor auth = html . HR.renderHtml . template auth . postsListHtml . filter (hasAuthor auth) =<< lift getAllPosts

getPageFromFile :: T.Text -> IO (Maybe Page.Page)
getPageFromFile title = do
  let path = T.unpack $ mconcat ["_pages/", title]
  getFromFile Page.toPage path

getPostFromFile :: T.Text -> T.Text -> IO (Maybe Post.Post)
getPostFromFile date title = do
  let postPath = T.unpack $ mconcat ["_posts/", date, "-", title, ".md"]
  getFromFile Post.toPost postPath

getFromFile :: (T.Text -> Maybe a) -> String -> IO (Maybe a)
getFromFile constructor path = do
  fileContent <- (T.decodeUtf8' <$> BSL.readFile path) `catchIOError` const (pure $ Left undefined)
  let cont = case fileContent of
              Left _  -> Nothing
              Right x -> Just x
  let content = constructor =<< cont
  return content

getAllTags :: [Post.Post] -> [T.Text]
getAllTags = getAll Post.tags

hasTag :: T.Text -> Post.Post -> Bool
hasTag tag = ([]/=) . filter (==tag) . Post.tags

getAllAuthors :: [Post.Post] -> [T.Text]
getAllAuthors = getAll Post.authors

getAll :: (Post.Post -> [T.Text]) -> [Post.Post] -> [T.Text]
getAll f  = L.sort . map (T.unwords . T.words . head) . L.group . L.sort . concatMap f

hasAuthor :: T.Text -> Post.Post -> Bool
hasAuthor auth myPost = auth `elem` Post.authors myPost

