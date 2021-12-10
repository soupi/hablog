{-# LANGUAGE OverloadedStrings #-}

module Web.Hablog.Post
  ( module Web.Hablog.Post
  , Text
  )
  where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Text.Blaze.Html5 as H
import qualified Data.Map as M
import Data.Time (fromGregorian, Day)

import Web.Hablog.Utils

data Preview
  = Preview
    { previewTitle :: Maybe Text
    , previewSummary :: Maybe Text
    , previewImage :: Maybe (FilePath, Text)
    , previewAuthor :: Maybe Text
    , previewSite :: Maybe Text
    }

data Post
  = Post
  { date  :: (Text, Text, Text)
  , route :: Text
  , title :: Text
  , authors :: [Text]
  , tags    :: [Text]
  , preview :: Preview
  , content :: H.Html
  }

year, month, day :: Post -> Text
year  p = case date p of { (y, _, _) -> y; }
month p = case date p of { (_, m, _) -> m; }
day   p = case date p of { (_, _, d) -> d; }

toDay :: Post -> Maybe Day
toDay post =
  case (reads $ T.unpack $ year post, reads $ T.unpack $ month post, reads $ T.unpack $ day post) of
    ([(y,[])], [(m,[])], [(d,[])]) -> pure (fromGregorian y m d)
    _ -> Nothing

toPost :: Text -> Maybe Post
toPost fileContent =
  Post <$> ((,,) <$> yyyy <*> mm <*> dd)
       <*> M.lookup "route" header
       <*> M.lookup "title" header
       <*> (map (T.unwords . T.words) . T.split (==',') <$> M.lookup "authors" header)
       <*> (map (T.toLower . T.unwords . T.words) . T.split (==',') <$> M.lookup "tags" header)
       <*> pure preview'
       <*> pure (createBody $ getContent fileContent)
    where
        header = getHeader fileContent
        dt     = T.split (=='-') <$> M.lookup "date" header
        yyyy   = dt >>= (`at` 0)
        mm     = dt >>= (`at` 1)
        dd     = dt >>= (`at` 2)
        preview' = mkPreview header

noPreview :: Preview
noPreview = Preview mempty mempty mempty mempty mempty

mkPreview :: M.Map Text Text -> Preview
mkPreview header =
  Preview
    (M.lookup "title" header)
    (M.lookup "summary" header)
    ( (,)
      <$> fmap T.unpack (M.lookup "image" header)
      <*> M.lookup "image-alt" header
    )
    (M.lookup "authors" header)
    (M.lookup "twitter_handle" header)

getPath :: Post -> Text
getPath post =
  T.concat ["post/", year post, "/", month post, "/", day post, "/", route post]

getDate :: Post -> Text
getDate post =
  T.concat [day post, "/", month post, "/", year post]

eqY, eqM, eqD :: Text -> Post -> Bool
eqY y p = y == year  p
eqM m p = m == month p
eqD d p = d == day   p

eqYM :: (Text, Text) -> Post -> Bool
eqYM (y,m) p = eqY y p && eqM m p

eqDate :: (Text, Text, Text) -> Post -> Bool
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
