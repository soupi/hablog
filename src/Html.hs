{-# LANGUAGE OverloadedStrings #-}

module Html where

import Data.Monoid (mconcat)
import Data.String (fromString)
import Data.List (sort)
import qualified Data.Text.Lazy as T
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A


import qualified Model as Post

template :: T.Text -> H.Html -> H.Html
template title container =
  H.docTypeHtml $ do
    H.head $ do
      H.title (H.toHtml title)
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/static/css/style.css"
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/static/highlight/styles/obsidian.css"
    H.body $ do
      H.div ! A.class_ "container" $ do
        logo
        H.div ! A.class_ "maincontainer" $ container
      H.script ! A.src "static/highlight/highlight.pack.js" $ ""
      H.script "hljs.initHighlightingOnLoad();"


mainTemplate :: H.Html -> H.Html
mainTemplate content = H.article ! A.class_ "content" $ content

notFoundPage :: H.Html
notFoundPage =
  template "Not Found" $ mainTemplate $ do
    H.h1 "Not found"
    H.p "The page you search for is not available."

logo :: H.Html
logo = H.header ! A.class_ "logo" $ H.h1 $ H.a ! A.href "/" $ "Hablog"

errorPage :: T.Text -> String -> H.Html
errorPage ttl msg =
  template ttl $ do
    H.h2 "Something Went Wrong..."
    H.p $ H.toHtml msg

emptyPage :: H.Html
emptyPage = H.span " "


postsListHtml :: [Post.Post] -> H.Html
postsListHtml posts =
   H.div ! A.class_ "PostsList" $ do 
    H.h1 "Posts"
    postsList posts

postsList :: [Post.Post] -> H.Html
postsList = H.ul . mconcat . fmap postsListItem

postsListItem :: Post.Post -> H.Html
postsListItem post = H.li $ do
  H.span ! A.class_ "postDate" $ H.toHtml $ Post.date post
  H.span ! A.class_ "seperator" $ " - "
  H.a ! A.href (fromString ("/" ++ Post.getPath post)) $ H.toHtml $ Post.headerTitle post

postPage :: Post.Post -> H.Html
postPage post = template (T.pack (Post.headerTitle post)) $ do
    H.article ! A.class_ "post" $ do
      H.div ! A.class_ "postTitle" $ do
        H.a ! A.href (fromString ("/" ++ Post.getPath post)) $ H.h2 ! A.class_ "postHeader" $ H.toHtml (Post.headerTitle post)
        H.span ! A.class_ "postSubTitle" $ do
          H.span ! A.class_ "postAuthor" $ H.toHtml $ authorsList $ Post.authors post
          H.span ! A.class_ "seperator" $ " - "
          H.span ! A.class_ "postDate" $ H.toHtml $ Post.date post
          H.span ! A.class_ "seperator" $ " - "
          H.span ! A.class_ "postTags" $ tagsList (Post.tags post)
      H.div ! A.class_ "postContent" $ Post.content post

tagsList :: [String] -> H.Html
tagsList = H.ul . mconcat . fmap tagsListItem . sort

tagsListItem :: String -> H.Html
tagsListItem tag = H.li $ H.a ! A.href (fromString ("/tags/" ++ tag)) $ H.toHtml tag

authorsList :: [String] -> H.Html
authorsList = H.ul . mconcat . fmap authorsListItem . sort

authorsListItem :: String -> H.Html
authorsListItem author = H.li $ H.a ! A.href (fromString ("/authors/" ++ author)) $ H.toHtml author



