{-# LANGUAGE OverloadedStrings #-}

module Present where

import           Web.Scotty
import           Data.Monoid (mconcat)
import           Control.Monad (liftM)
import           Control.Monad.Trans.Class (lift)
import qualified Data.List as L
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO

import qualified Text.Blaze.Html.Renderer.Text as HR
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html5 ((!))

import qualified System.Directory as DIR (getDirectoryContents)

import Html
import qualified Model as Post

presentMain :: ActionM ()
presentMain = do
  allPosts <- lift getAllPosts
  tgs <- lift getTagList
  auths <- lift getAuthorsList
  html $ HR.renderHtml $ template "Hablog" $ do
  H.aside ! A.class_ "aside" $ do
    H.div ! A.class_ "AllAuthorsList" $ do
      H.h1 "Authors"
      auths
    H.div ! A.class_ "AllTagsList" $ do
      H.h1 "Tags"
      tgs
  postsListHtml allPosts

getAllPosts :: IO [Post.Post]
getAllPosts = do
  posts <- liftM (drop 2) (DIR.getDirectoryContents "_posts")
  contents <- mapM (TIO.readFile . ("_posts/"++)) posts
  return $ map (uncurry Post.toPost) $ reverse (zip posts contents)

presentPosts :: T.Text -> T.Text -> ActionM ()
presentPosts search query = html $ mconcat ["posts with: ", search, " and ", query]

presentPost :: T.Text -> T.Text -> ActionM ()
presentPost date title = do
  myPost <- lift $ getPostFromFile date title
  html $ HR.renderHtml $ postPage myPost

presentTags :: ActionM ()
presentTags = html . HR.renderHtml . template "Posts Tags" =<< lift getTagList

getTagList :: IO H.Html
getTagList = return . tagsList . getAllTags =<< getAllPosts

getAuthorsList :: IO H.Html
getAuthorsList = return . authorsList . getAllAuthors =<< getAllPosts

presentTag :: String -> ActionM ()
presentTag tag = html . HR.renderHtml . template (T.pack tag) . postsListHtml . filter (hasTag tag) =<< lift getAllPosts

presentAuthors :: ActionM ()
presentAuthors = html . HR.renderHtml . template "Posts Authors" =<< lift getAuthorsList

presentAuthor :: String -> ActionM ()
presentAuthor auth = html . HR.renderHtml . template (T.pack auth) . postsListHtml . filter (hasAuthor auth) =<< lift getAllPosts

getPostFromFile :: T.Text -> T.Text -> IO Post.Post
getPostFromFile date title = do
  let postPath = T.unpack $ mconcat ["_posts/", date, "-", title, ".md"]
  fileContent <- TIO.readFile postPath
  let myPost = Post.toPost postPath fileContent
  return myPost

getAllTags :: [Post.Post] -> [String]
getAllTags = L.sort . map (Post.removeWhitespaces . head) . L.group . L.sort . concatMap Post.tags

hasTag :: String -> Post.Post -> Bool
hasTag tag = ([]/=) . filter (==tag) . Post.tags

getAllAuthors :: [Post.Post] -> [String]
getAllAuthors = L.sort . map (Post.removeWhitespaces . head) . L.group . L.sort . map Post.author

hasAuthor :: String -> Post.Post -> Bool
hasAuthor auth myPost = auth == Post.author myPost
