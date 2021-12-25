{-# language ViewPatterns #-}
{-# language OverloadedStrings #-}

module Web.Hablog.Post
  ( module Web.Hablog.Post
  , Text
  , UTCTime
  )
  where

import Text.Read (readMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Text.Blaze.Html5 as H
import qualified Data.Map as M
import Data.Time (fromGregorian, Day, UTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)

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
    { postDate  :: Day
    , postRoute :: Text
    , postTitle :: Text
    , postAuthors :: [Text]
    , postTags    :: [Text]
    , postPreview :: Preview
    , postIsPreview :: Bool
    , postModificationTime :: UTCTime
    , postContent :: H.Html
    }

year, month, day :: Post -> Text
year  p = T.pack $ take 4 $ iso8601Show $ postDate p
month p = T.pack $ take 2 $ drop 5 $ iso8601Show $ postDate p
day   p = T.pack $ take 2 $ drop 8 $ iso8601Show $ postDate p

toDay :: Post -> Maybe Day
toDay post =
  case (reads $ T.unpack $ year post, reads $ T.unpack $ month post, reads $ T.unpack $ day post) of
    ([(y,[])], [(m,[])], [(d,[])]) -> pure (fromGregorian y m d)
    _ -> Nothing

toPost :: UTCTime -> Text -> Maybe Post
toPost modtime fileContent =
  Post
    <$> dt
    <*> M.lookup "route" header
    <*> M.lookup "title" header
    <*> (map (T.unwords . T.words) . T.split (==',') <$> M.lookup "authors" header)
    <*> (map (T.toLower . T.unwords . T.words) . T.split (==',') <$> M.lookup "tags" header)
    <*> pure preview'
    <*> pure isPreview
    <*> pure modtime
    <*> pure (createBody $ getContent fileContent)
  where
    dt = readMaybe . T.unpack =<< M.lookup "date" header
    header = getHeader fileContent
    preview' = mkPreview header
    isPreview =
      case cleanText <$> M.lookup "preview" header of
        Just "true" -> True
        _ -> False

cleanText :: Text -> String
cleanText = T.unpack . T.toLower . T.unwords . T.words

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
  T.concat ["post/", year post, "/", month post, "/", day post, "/", postRoute post]

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
eqDate (y,m,d) p =
  eqY y p && eqM m p && eqD d p

instance Show Post where
  show post =
    T.unpack $ T.concat ["post/", year post, "/", month post, "/", day post, "/", postRoute post]

instance Eq Post where
  (==) p1 p2 = postRoute p1 == postRoute p2


instance Ord Post where
  compare p1 p2
    | year p1 < year p2 = LT
    | year p1 == year p2 && month p1 < month p2 = LT
    | year p1 == year p2 && month p1 == month p2 && day p1 < day p2 = LT
    | year p1 == year p2 && month p1 == month p2 && day p1 == day p2 = EQ
    | otherwise = GT
