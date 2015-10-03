{-# LANGUAGE OverloadedStrings #-}

module Hablog.Present where

import           Web.Scotty
import           Control.Monad (liftM)
import           Control.Monad.Trans.Class (lift)
import           Data.Maybe (catMaybes)
import qualified Data.List as L
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO

import qualified Text.Blaze.Html.Renderer.Text as HR
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html5 ((!))

import qualified System.Directory as DIR (getDirectoryContents)
import           System.IO.Error (catchIOError)

import Hablog.Utils (removeWhitespaces)
import Hablog.Html
import qualified Hablog.Model as Model
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
presentPagesList pages = do
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
getAllPages = do
  pages <- liftM (L.delete ".." . L.delete ".") (DIR.getDirectoryContents "_pages" `catchIOError` (\_ -> return []))
  contents <- catMaybes <$> mapM ((\x -> (pure <$> TIO.readFile x) `catchIOError` const (pure Nothing)) . ("_pages/"++)) pages
  return . L.sortBy (flip compare) . catMaybes $ fmap Page.toPage (reverse contents)

getAllPosts :: IO [Model.Post]
getAllPosts = do
  posts <- liftM (L.delete ".." . L.delete ".") (DIR.getDirectoryContents "_posts")
  contents <- catMaybes <$> mapM ((\x -> (pure <$> TIO.readFile x) `catchIOError` const (pure Nothing)) . ("_posts/"++)) posts
  return . L.sortBy (flip compare) . catMaybes $ fmap (uncurry Model.toPost) (reverse (zip posts contents))

presentPost :: T.Text -> T.Text -> ActionM ()
presentPost date title = do
  myPost <- lift $ getPostFromFile date title
  case postPage <$> myPost of
    Just p -> html $ HR.renderHtml p
    Nothing -> html $ HR.renderHtml $ errorPage "Hablog - 404: not found" "Could not find the page you were looking for."

presentTags :: ActionM ()
presentTags = html . HR.renderHtml . template "Posts Tags" =<< lift getTagList

getTagList :: IO H.Html
getTagList = return . tagsList . getAllTags =<< getAllPosts

getPageList :: [Page.Page] -> H.Html
getPageList = pagesList


getAuthorsList :: IO H.Html
getAuthorsList = return . authorsList . getAllAuthors =<< getAllPosts

presentTag :: String -> ActionM ()
presentTag tag = html . HR.renderHtml . template (T.pack tag) . postsListHtml . filter (hasTag tag) =<< lift getAllPosts

presentAuthors :: ActionM ()
presentAuthors = html . HR.renderHtml . template "Posts Authors" =<< lift getAuthorsList

presentAuthor :: String -> ActionM ()
presentAuthor auth = html . HR.renderHtml . template (T.pack auth) . postsListHtml . filter (hasAuthor auth) =<< lift getAllPosts

getPageFromFile :: T.Text -> IO (Maybe Page.Page)
getPageFromFile title = do
  let path = T.unpack $ mconcat ["_pages/", title]
  getFromFile (const Page.toPage) path

getPostFromFile :: T.Text -> T.Text -> IO (Maybe Model.Post)
getPostFromFile date title = do
  let postPath = T.unpack $ mconcat ["_posts/", date, "-", title, ".md"]
  getFromFile Model.toPost postPath

getFromFile :: (String -> T.Text -> Maybe a) -> String -> IO (Maybe a)
getFromFile constructor path = do
  fileContent <- (pure <$> TIO.readFile path) `catchIOError` const (pure Nothing)
  let content = constructor path =<< fileContent
  return content

getAllTags :: [Model.Post] -> [String]
getAllTags = L.sort . map (removeWhitespaces . head) . L.group . L.sort . concatMap Model.tags

hasTag :: String -> Model.Post -> Bool
hasTag tag = ([]/=) . filter (==tag) . Model.tags

getAllAuthors :: [Model.Post] -> [String]
getAllAuthors = L.sort . map (removeWhitespaces . head) . L.group . L.sort . concatMap Model.authors


hasAuthor :: String -> Model.Post -> Bool
hasAuthor auth myPost = auth `elem` Model.authors myPost
