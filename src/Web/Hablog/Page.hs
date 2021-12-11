{-# LANGUAGE OverloadedStrings #-}

module Web.Hablog.Page where

import           Control.Arrow  ((&&&))
import qualified Data.Map as M
import qualified Data.Text.Lazy as T
import qualified Text.Blaze.Html5 as H

import Web.Hablog.Utils
import Web.Hablog.Post

data Page
    = Page
    { pageURL     :: FilePath
    , pageName    :: T.Text
    , pagePreview :: Preview
    , pageModificationTime :: UTCTime
    , pageContent :: H.Html
    }

toPage :: UTCTime -> T.Text -> Maybe Page
toPage modtime fileContent =
  Page <$> fmap T.unpack (M.lookup "route" header)
       <*> M.lookup "title" header
       <*> pure (mkPreview header)
       <*> pure modtime
       <*> pure (createBody content')
    where (header, content') = (getHeader &&& getContent) fileContent


instance Show Page where
  show = pageURL

instance Eq Page where
  (==) p1 p2 = pageURL p1 == pageURL p2

instance Ord Page where
  compare p1 p2
    | pageName p1 < pageName p2 = LT
    | otherwise = GT
