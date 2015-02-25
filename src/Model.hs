{-# LANGUAGE OverloadedStrings #-}

module Model where

import           Control.Applicative ((<$>),(<*>), pure)
import           Data.Char (toLower)
import qualified Data.Text.Lazy as T (Text, pack, unpack, lines, unlines, concat)
import qualified Text.Markdown as MD
import qualified Text.Blaze.Html5 as H

import Utils

data Post = Post { year :: String
                 , month :: String
                 , day :: String
                 , pathTitle :: String
                 , headerTitle :: String
                 , authors :: [String]
                 , tags :: [String]
                 , content :: H.Html }

toPost :: String -> T.Text -> Maybe Post
toPost path fileContent = Post <$> yyyy <*> mm <*> dd <*> pttl <*> httl <*> auth <*> tgs <*> pure (MD.markdown MD.def (T.unlines (dropWhile (/="") (T.lines fileContent))))
  where as_list = splitBy '-' path
        yyyy    = (\x -> if length x > 1 then x `at` 1 else hd x) =<< fmap (splitBy '/') (hd as_list)
        mm      = as_list `at` 1
        dd      = as_list `at` 2
        pttl    = pure $ reverse $ drop 3 $ reverse $ convert '-' $ drop 3 as_list
        header  = takeWhile (/=[]) . lines . T.unpack $ fileContent
        getHd p = takeJust $ fmap ((\x -> if hd x == Just p then Just (unwords (tail x)) else Nothing) . words) header
        httl    = getHd "title:"
        auth    = getList "authors:"
        tgs     = map (map toLower) <$> getList "tags:"
        getList x = map removeWhitespaces . splitBy ',' <$> getHd x

getPath :: Post -> String
getPath post = concat ["post/", year post, "/", month post, "/", day post, "/", (pathTitle) post]

date :: Post -> T.Text
date post = T.concat $ map T.pack [day post, "/", month post, "/", year post]

instance Show Post where
  show post = concat ["post/", year post, "/", month post, "/", day post, "/", (pathTitle) post]

instance Eq Post where
  (==) p1 p2 = pathTitle p1 == pathTitle p2


instance Ord Post where
  compare p1 p2
    | year p1 < year p2 = LT
    | year p1 == year p2 && month p1 < month p2 = LT
    | year p1 == year p2 && month p1 == month p2 && day p1 < day p2 = LT
    | year p1 == year p2 && month p1 == month p2 && day p1 == day p2 = EQ
    | otherwise = GT
