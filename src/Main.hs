{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Web.Scotty
import           Data.Monoid (mconcat)
import           Control.Monad (liftM)
import qualified Data.Text.Lazy as T

import Present

main :: IO ()
main = scotty 3000 router

router :: ScottyM ()
router = do
  get "/" presentMain
  get "/posts/:search/:query" $ do
    search <- param "search"
    query <- param "query"
    presentPosts search query
  get "/post/:yyyy/:mm/:dd/:title" $ do
    yyyy <- param "yyyy"
    mm <- param "mm"
    dd <- param "dd"
    title <- param "title"
    presentPost (mconcat [yyyy,"-",mm,"-",dd]) title
  get (regex "/static/(.*)") $ do
    path <- liftM (drop 1 . T.unpack) (param "0")
    if hasdots path then
      fail "no dots in path allowed"
      else file path

hasdots :: String -> Bool
hasdots [] = False
hasdots ('.':'.':_) = True
hasdots (_:rest) = hasdots rest
