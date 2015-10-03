{-# LANGUAGE OverloadedStrings #-}

module Hablog.Post where

import qualified Data.Text.Lazy as T
import qualified Text.Blaze.Html5 as H
import qualified Data.Map as M

import Hablog.Utils


data Post
  = Post
  { date  :: (T.Text, T.Text, T.Text)
  , route :: T.Text
  , title :: T.Text
  , authors :: [T.Text]
  , tags    :: [T.Text]
  , content :: H.Html
  }

year, month, day :: Post -> T.Text
year  p = case date p of { (y, _, _) -> y; }
month p = case date p of { (_, m, _) -> m; }
day   p = case date p of { (_, _, d) -> d; }

toPost :: T.Text -> Maybe Post
toPost fileContent =
  Post <$> ((,,) <$> yyyy <*> mm <*> dd)
       <*> M.lookup "route" header
       <*> M.lookup "title" header
       <*> (map (T.unwords . T.words) . T.split (==',') <$> M.lookup "authors" header)
       <*> (map (T.toLower . T.unwords . T.words) . T.split (==',') <$> M.lookup "tags" header)
       <*> pure (createBody $ getContent fileContent)
    where
        header = getHeader fileContent
        dt     = T.split (=='-') <$> M.lookup "date" header
        yyyy   = dt >>= (`at` 0)
        mm     = dt >>= (`at` 1)
        dd     = dt >>= (`at` 2)

getPath :: Post -> T.Text
getPath post =
  T.concat ["post/", year post, "/", month post, "/", day post, "/", route post]

getDate :: Post -> T.Text
getDate post =
  T.concat [day post, "/", month post, "/", year post]

eqY, eqM, eqD :: T.Text -> Post -> Bool
eqY y p = y == year  p
eqM m p = m == month p
eqD d p = d == day   p

eqYM :: (T.Text, T.Text) -> Post -> Bool
eqYM (y,m) p = eqY y p && eqM m p

eqDate :: (T.Text, T.Text, T.Text) -> Post -> Bool
eqDate dt p = dt == date p

instance Show Post where
  show post =
    T.unpack $ T.concat ["post/", year post, "/", month post, "/", day post, "/", route post]

instance Eq Post where
  (==) p1 p2 = route p1 == route p2


instance Ord Post where
  compare p1 p2
    | year p1 < year p2 = LT
    | year p1 == year p2 && month p1 < month p2 = LT
    | year p1 == year p2 && month p1 == month p2 && day p1 < day p2 = LT
    | year p1 == year p2 && month p1 == month p2 && day p1 == day p2 = EQ
    | otherwise = GT

