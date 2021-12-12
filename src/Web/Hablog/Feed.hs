{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}

-- | Atom and RSS feeds
module Web.Hablog.Feed where

import Web.Hablog.Config
import Web.Hablog.Post
import qualified Data.Set as S
import qualified Text.Blaze.Html.Renderer.Text as HR

import Data.Time
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Network.URI.Encode (encodeText)
import Text.Feed.Types

import qualified Text.RSS.Syntax as RSS
import qualified Text.Atom.Feed as Atom
import qualified Text.Feed.Export as Export (textFeed)

-- * RSS feed

renderRSSFeed :: RSS.RSS -> TL.Text
renderRSSFeed = fromJust . Export.textFeed . RSSFeed

rssFeed :: Config -> [Post] -> RSS.RSS
rssFeed cfg posts =
  let
    domain = blogDomain cfg
  in
    RSS.RSS
      { RSS.rssVersion = "2.0"
      , RSS.rssChannel =
        (RSS.nullChannel (TL.toStrict $ blogTitle cfg) (TL.toStrict domain))
          { RSS.rssItems =
            map (rssItem cfg) posts
          , RSS.rssCategories =
            uniques (tagToCatRSS domain) postTags posts
          , RSS.rssLastUpdate =
            Just $ lastUpdated posts
          , RSS.rssImage =
            Just $ RSS.nullImage
              (TL.toStrict $ domain <> "/favicon.ico")
              (TL.toStrict $ blogTitle cfg)
              (TL.toStrict domain)
          }
      , RSS.rssAttrs = []
      , RSS.rssOther = []
      }

rssItem :: Config -> Post -> RSS.RSSItem
rssItem Config{..} post@Post{..} =
  (RSS.nullItem $ TL.toStrict postTitle)
    { RSS.rssItemLink =
      Just $ TL.toStrict (blogDomain <> "/" <> getPath post)
    , RSS.rssItemDescription =
      -- TL.toStrict <$> previewSummary postPreview
      Just $ TL.toStrict $ HR.renderHtml postContent
    , RSS.rssItemCategories =
      tagToCatRSS blogDomain <$> postTags
    , RSS.rssItemPubDate =
      Just $ showDayRfc822 postDate
    -- , RSS.rssItemContent =
    --   Just $ TL.toStrict $ HR.renderHtml postContent
    }

tagToCatRSS :: TL.Text -> TL.Text -> RSS.RSSCategory
tagToCatRSS domain tag =
  RSS.RSSCategory
    { RSS.rssCategoryValue = TL.toStrict tag
    , RSS.rssCategoryDomain = Just $ TL.toStrict domain <> "/tags/" <> encodeText (TL.toStrict tag)
    , RSS.rssCategoryAttrs = []
    }

-- * Atom feed

renderAtomFeed :: Atom.Feed -> TL.Text
renderAtomFeed = fromJust . Export.textFeed . AtomFeed

atomFeed :: Config -> [Post] -> Atom.Feed
atomFeed cfg posts =
  let
    domain = blogDomain cfg
    initFeed =
      Atom.nullFeed
        (TL.toStrict $ domain <> "/blog/atom.xml")
        (Atom.TextString . TL.toStrict . blogTitle $ cfg)
        (lastUpdated posts)
  in
    initFeed
      { Atom.feedEntries =
        map (toAtomEntry domain) posts
      , Atom.feedAuthors =
        uniques (toPerson domain) postAuthors posts
      , Atom.feedCategories =
        uniques (tagToCat domain) postTags posts
      , Atom.feedIcon =
        Just . TL.toStrict $ domain <> "/favicon.ico"
      , Atom.feedLogo =
        Just . TL.toStrict $ domain <> "/favicon.ico"
      }

toAtomEntry :: Text -> Post -> Atom.Entry
toAtomEntry domain post@Post{..} =
  Atom.Entry
    { Atom.entryId = TL.toStrict (domain <> "/" <> getPath post)
    , Atom.entryTitle = Atom.TextString $ TL.toStrict postTitle
    , Atom.entryUpdated = showDayRfc822 postDate
    , Atom.entryAuthors =
      map (toPerson domain) postAuthors
    , Atom.entryCategories =
      map (tagToCat domain) postTags
    , Atom.entryContent =
      Just $ Atom.HTMLContent $ TL.toStrict $ HR.renderHtml postContent
    , Atom.entryContributor = []
    , Atom.entryLinks =
      [ Atom.nullLink (TL.toStrict $ domain <> "/" <> getPath post)
      ]
    , Atom.entryPublished = Just $ T.pack $ show postDate
    , Atom.entryRights = Nothing
    , Atom.entrySource = Nothing
    , Atom.entrySummary =
      Atom.TextString . TL.toStrict <$> previewSummary postPreview
    , Atom.entryInReplyTo = Nothing
    , Atom.entryInReplyTotal = Nothing
    , Atom.entryAttrs = []
    , Atom.entryOther = []
    }

toPerson :: TL.Text -> TL.Text -> Atom.Person
toPerson domain name =
  Atom.nullPerson
    { Atom.personName = TL.toStrict name
    , Atom.personURI  = Just $ TL.toStrict domain
    , Atom.personEmail = Nothing
    }

tagToCat :: TL.Text -> TL.Text -> Atom.Category
tagToCat domain tag =
  (Atom.newCategory $ TL.toStrict tag)
    { Atom.catLabel = Just $ TL.toStrict tag
    , Atom.catScheme = Just $ TL.toStrict domain <> "/tags/" <> encodeText (TL.toStrict tag)
    }

-- * Utils

uniques :: Ord b => (b -> c) -> (a -> [b]) -> [a] -> [c]
uniques convert extract =
  ( map convert
  . S.toList
  . S.unions
  . map
    ( S.fromList
    . extract
    )
  )

lastUpdated :: [Post] -> T.Text
lastUpdated = T.pack . show . maximum . map postModificationTime

showDayRfc822 :: Day -> T.Text
showDayRfc822 d =
  T.pack $ formatTime defaultTimeLocale rfc822DateFormat $ (UTCTime d (secondsToDiffTime 0))
