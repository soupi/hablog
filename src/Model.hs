{-# LANGUAGE OverloadedStrings #-}

module Model (toPost,year,month,day,title, Post) where

data Post = Post { year :: String, month :: String, day :: String, title :: String } deriving (Eq)

toPost :: String -> Post
toPost txt = Post yyyy mm dd ttl
  where as_list = splitBy '-' txt
        yyyy   = as_list !! 0
        mm     = as_list !! 1
        dd     = as_list !! 2
        ttl    = reverse $ drop 3 $ reverse $ convert ' ' $ drop 3 as_list

convert :: Char -> [String] -> String
convert c str = concatMap (++[c]) (init str) ++ last str

splitBy :: Char -> String -> [String]
splitBy c txt = map reverse $ go [] txt
  where go xs [] = [xs]
        go xs (y:ys)
          | y == c    = xs : go [] ys
          | otherwise = go (y:xs) ys

instance Show Post where
  show post = concat ["post/", year post, "/", month post, "/", day post, "/", (convert '-' . splitBy ' ' . title) post]
