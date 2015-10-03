{-# LANGUAGE OverloadedStrings #-}

module Hablog.Html where

import Data.String (fromString)
import Data.List (sort)
import qualified Data.Text.Lazy as T
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A


import Hablog.Settings
import qualified Hablog.Post as Post
import qualified Hablog.Page as Page

template :: T.Text -> H.Html -> H.Html
template title container =
  H.docTypeHtml $ do
    H.head $ do
      H.title (H.toHtml (T.concat [blogTitle, " - ", title]))
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href (bgTheme blogTheme)
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href (codeTheme blogTheme)
    H.body $ do
      H.div ! A.class_ "container" $ do
        logo
        H.div ! A.class_ "maincontainer" $ container
        footer
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
logo = H.header ! A.class_ "logo" $ H.h1 $ H.a ! A.href "/" $ H.toHtml blogTitle

footer :: H.Html
footer = H.footer ! A.class_ "footer" $ do
    H.span "Powered by "
    H.a ! A.href "https://github.com/soupi/hablog" $ "Hablog"

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
  H.span ! A.class_ "postDate" $ H.toHtml $ Post.getDate post
  H.span ! A.class_ "seperator" $ " - "
  H.a ! A.href (fromString $ T.unpack ("/" `T.append` Post.getPath post)) $ H.toHtml $ Post.title post

postPage :: Post.Post -> H.Html
postPage post = template (Post.title post) $
    H.article ! A.class_ "post" $ do
      H.div ! A.class_ "postTitle" $ do
        H.a ! A.href (fromString $ T.unpack ("/" `T.append` Post.getPath post)) $ H.h2 ! A.class_ "postHeader" $ H.toHtml (Post.title post)
        H.span ! A.class_ "postSubTitle" $ do
          H.span ! A.class_ "postAuthor" $ H.toHtml $ authorsList $ Post.authors post
          H.span ! A.class_ "seperator" $ " - "
          H.span ! A.class_ "postDate" $ H.toHtml $ Post.getDate post
          H.span ! A.class_ "seperator" $ " - "
          H.span ! A.class_ "postTags" $ tagsList (Post.tags post)
      H.div ! A.class_ "postContent" $ Post.content post

pagePage :: Page.Page -> H.Html
pagePage page = template (Page.getPageName page) $
    H.article ! A.class_ "post" $ do
      H.div ! A.class_ "postTitle" $
        H.a ! A.href (fromString (Page.getPageURL page)) $ H.h2 ! A.class_ "postHeader" $ H.toHtml (Page.getPageName page)
      H.div ! A.class_ "postContent" $ Page.getPageContent page


pagesList :: [Page.Page] -> H.Html
pagesList = H.ul . mconcat . fmap pagesListItem . sort

pagesListItem :: Page.Page -> H.Html
pagesListItem page = H.li $ H.a ! A.href (fromString ("/page/" ++ Page.getPageURL page)) $ H.toHtml (Page.getPageName page)

tagsList :: [T.Text] -> H.Html
tagsList = H.ul . mconcat . fmap tagsListItem . sort

tagsListItem :: T.Text -> H.Html
tagsListItem tag = H.li $ H.a ! A.href (fromString $ T.unpack ("/tags/" `T.append` tag)) $ H.toHtml tag

authorsList :: [T.Text] -> H.Html
authorsList = H.ul . mconcat . fmap authorsListItem . sort

authorsListItem :: T.Text -> H.Html
authorsListItem author = H.li $ H.a ! A.href (fromString $ T.unpack ("/authors/" `T.append` author)) $ H.toHtml author



