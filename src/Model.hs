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
                 , authors :: [String]
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
        getList = map removeWhitespaces . splitBy ',' . fromMaybe "" . getHd
        auth    = getList "authors:"
        tgs     = map (map toLower) $ getList "tags:"

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


getPath :: Post -> String
getPath post = concat ["post/", year post, "/", month post, "/", day post, "/", (convert '-' . splitBy ' ' . pathTitle) post]

removeWhitespaces :: String -> String
removeWhitespaces = unwords . words

date :: Post -> T.Text
date post = T.concat $ map T.pack [day post, "/", month post, "/", year post]

instance Show Post where
  show post = concat ["post/", year post, "/", month post, "/", day post, "/", (convert '-' . splitBy ' ' . pathTitle) post]

instance Eq Post where
  (==) p1 p2 = pathTitle p1 == pathTitle p2


instance Ord Post where
  compare p1 p2
    | year p1 < year p2 = LT
    | year p1 == year p2 && month p1 < month p2 = LT
    | year p1 == year p2 && month p1 == month p2 && day p1 < day p2 = LT
    | year p1 == year p2 && month p1 == month p2 && day p1 == day p2 = EQ
    | otherwise = GT
