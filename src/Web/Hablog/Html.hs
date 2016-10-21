{-# LANGUAGE OverloadedStrings #-}

module Web.Hablog.Html where

import Data.String (fromString)
import Data.List (sort)
import qualified Data.Text.Lazy as T
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A


import Web.Hablog.Config
import qualified Web.Hablog.Post as Post
import qualified Web.Hablog.Page as Page

template :: Config -> Bool -> T.Text -> H.Html -> H.Html
template cfg highlight title container =
  H.docTypeHtml $ do
    H.head $ do
      H.title (H.toHtml (T.concat [blogTitle cfg, " - ", title]))
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href (bgTheme $ blogTheme cfg)
      if highlight
        then H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href (codeTheme $ blogTheme cfg)
        else mempty
    H.body $ do
      H.div ! A.class_ "container" $ do
        logo cfg
        H.div ! A.class_ "maincontainer" $ container
        footer
      if highlight
        then do
          H.script ! A.src "static/highlight/highlight.pack.js" $ ""
          H.script "hljs.initHighlightingOnLoad();"
        else
          mempty

mainTemplate :: H.Html -> H.Html
mainTemplate = H.article ! A.class_ "content"

notFoundPage :: Config -> H.Html
notFoundPage cfg =
  template cfg False "Not Found" $ mainTemplate $ do
    H.h1 "Not found"
    H.p "The page you search for is not available."

logo :: Config -> H.Html
logo cfg = H.header ! A.class_ "logo" $ H.h1 $ H.a ! A.href "/" $ H.toHtml (blogTitle cfg)

footer :: H.Html
footer = H.footer ! A.class_ "footer" $ do
    H.span "Powered by "
    H.a ! A.href "https://github.com/soupi/hablog" $ "Hablog"

errorPage :: Config -> T.Text -> String -> H.Html
errorPage cfg ttl msg =
  template cfg False ttl $ do
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

postPage :: Config -> Post.Post -> H.Html
postPage cfg post = template cfg True (Post.title post) $
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

pagePage :: Config -> Page.Page -> H.Html
pagePage cfg page = template cfg True (Page.getPageName page) $
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



