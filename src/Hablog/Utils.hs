{-# LANGUAGE OverloadedStrings #-}

module Hablog.Utils where

import Control.Arrow ((&&&))
import qualified Data.Text.Lazy as T

hd :: [a] -> Maybe a
hd [] = Nothing
hd (x:_) = Just x

at :: [a] -> Int -> Maybe a
at [] _ = Nothing
at (x:xs) n
  | n > 0 = xs `at` (n-1)
  | n == 0 = Just x
  | otherwise = reverse xs `at` (-n)

headerBreaker :: T.Text
headerBreaker = "---"

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

removeWhitespaces :: String -> String
removeWhitespaces = unwords . words

infixl 0 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x

partition :: Char -> T.Text -> (T.Text, T.Text)
partition c = T.takeWhile (/=c) &&& (T.unwords . T.words . T.dropWhile (==c) . T.dropWhile (/=c))

