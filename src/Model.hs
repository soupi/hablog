{-# LANGUAGE OverloadedStrings #-}

module Model where

import         Data.Maybe (fromMaybe)
import         Data.Char (toLower)
import qualified Data.Text.Lazy as T (Text, pack, unpack, lines, unlines, concat)
import qualified Text.Markdown as MD
import qualified Text.Blaze.Html5 as H

data Post = Post { year :: String
                 , month :: String
                 , day :: String
                 , pathTitle :: String
                 , headerTitle :: String
                 , author :: String
                 , tags :: [String]
                 , content :: H.Html }

toPost :: String -> T.Text -> Post
toPost path fileContent = Post yyyy mm dd pttl httl auth tgs $ MD.markdown MD.def $ T.unlines $ dropWhile (/="") $ T.lines fileContent
  where as_list = splitBy '-' path
        yyyy    = if length x > 1 then x !! 1 else head x where x = splitBy '/' $ head as_list 
        mm      = as_list !! 1
        dd      = as_list !! 2
        pttl    = reverse $ drop 3 $ reverse $ convert ' ' $ drop 3 as_list
        header  = takeWhile (/=[]) $ (lines . T.unpack) fileContent
        getHd p = takeJust $ map ((\x -> if hd x == Just p then Just (unwords (tail x)) else Nothing) . words) header
        httl    = fromMaybe "" $ getHd "title:"
        auth    = fromMaybe "" $ getHd "author:"
        tgs     = map removeWhitespaces $ splitBy ',' $ map toLower $ fromMaybe "" $ getHd "tags:"

hd :: [a] -> Maybe a
hd [] = Nothing
hd (x:_) = Just x

takeJust :: [Maybe a] -> Maybe a
takeJust [] = Nothing
takeJust (Just x:_) = Just x
takeJust (Nothing:xs) = takeJust xs

convert :: Char -> [String] -> String
convert c str = concatMap (++[c]) (init str) ++ last str

splitBy :: Char -> String -> [String]
splitBy c txt = map reverse $ go [] txt
  where go xs [] = [xs]
        go xs (y:ys)
          | y == c    = xs : go [] ys
          | otherwise = go (y:xs) ys

instance Show Post where
  show post = concat ["post/", year post, "/", month post, "/", day post, "/", (convert '-' . splitBy ' ' . pathTitle) post]

getPath :: Post -> String
getPath post = concat ["post/", year post, "/", month post, "/", day post, "/", (convert '-' . splitBy ' ' . pathTitle) post]

removeWhitespaces :: String -> String
removeWhitespaces = unwords . words

date :: Post -> T.Text
date post = T.concat $ map T.pack [day post, "/", month post, "/", year post]
