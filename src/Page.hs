{-# LANGUAGE OverloadedStrings #-}

module Page where

import           Control.Applicative ((<$>),(<*>), pure)
import qualified Data.Text.Lazy as T (Text, unpack, lines, unlines, concat)
import qualified Text.Markdown as MD
import qualified Text.Blaze.Html5 as H

import Utils

data Page = Page { getPagePath :: String
                 , getPageName :: String
                 , getPageContent :: H.Html }

toPage :: String -> T.Text -> Maybe Page
toPage path fileContent = Page <$> pure pathTtl <*> pageName <*> pure (MD.markdown MD.def (T.unlines (dropWhile (/="") (T.lines fileContent))))
  where header  = takeWhile (/=[]) . lines . T.unpack $ fileContent
        pathTtl = reverse $ drop 3 $ reverse path
        getHd p = takeJust $ fmap ((\x -> if hd x == Just p then Just (unwords (tail x)) else Nothing) . words) header
        pageName  = getHd "name:"

getPath :: Page -> String
getPath page = concat ["page/", (convert '-' . splitBy ' ' . getPagePath) page]

instance Show Page where
  show = getPath

instance Eq Page where
  (==) p1 p2 = getPageName p1 == getPageName p2


instance Ord Page where
  compare p1 p2
    | getPagePath p1 < getPagePath p2 = LT
    | otherwise = GT
