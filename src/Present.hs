{-# LANGUAGE OverloadedStrings #-}

module Present where

import           Web.Scotty
import           Data.Monoid (mconcat)
import           Control.Monad (liftM)
import           Control.Monad.Trans.Class (lift)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO

import qualified Text.Markdown as MD
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html.Renderer.Text as HR

import Html

presentMain :: ActionM ()
presentMain = html "main"

presentPosts :: T.Text -> T.Text -> ActionM ()
presentPosts search query = html $ mconcat ["posts with: ", search, " and ", query]

presentPost :: T.Text -> T.Text -> ActionM ()
presentPost date title = do
  content <- lift $ getMDFromFile date title
  html content

getMDFromFile :: T.Text -> T.Text -> IO T.Text
getMDFromFile date title = do
  fileContent <- liftM T.lines $ TIO.readFile $ T.unpack $ mconcat ["_posts/", date, "-", title, ".md"]
  let page_title = T.unwords $ drop 1 $ T.words $ head fileContent
  return $ HR.renderHtml $ template page_title $ MD.markdown MD.def $ T.unlines $ drop 2 fileContent

