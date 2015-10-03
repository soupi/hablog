{-# LANGUAGE OverloadedStrings #-}

module Hablog.Page where

import           Control.Arrow  ((&&&))
import qualified Data.Map as M
import qualified Data.Text.Lazy as T
import qualified Text.Blaze.Html5 as H

import Hablog.Utils

data Page
    = Page
    { getPageURL     :: FilePath
    , getPageName    :: T.Text
    , getPageContent :: H.Html
    }

toPage :: T.Text -> Maybe Page
toPage fileContent =
  Page <$> fmap T.unpack (M.lookup "route" header)
       <*> M.lookup "title" header
       <*> pure (createBody content)
    where (header, content) = (getHeader &&& getContent) fileContent


instance Show Page where
  show = getPageURL

instance Eq Page where
  (==) p1 p2 = getPageURL p1 == getPageURL p2

instance Ord Page where
  compare p1 p2
    | getPageName p1 < getPageName p2 = LT
    | otherwise = GT

