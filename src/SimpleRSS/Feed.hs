{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module SimpleRSS.Feed
    ( getChannels
    , Channel
    ) where

import Control.Applicative
import Data.Aeson hiding (json)
import Data.Maybe
import Data.Text (Text, pack)
import Data.UUID
import System.Directory
import System.FilePath
import Text.Feed.Import
import Text.Feed.Types hiding (RSSItem, Item)
import Text.RSS.Syntax

data Channel = Channel
    { channelId :: UUID
    , name :: Text
    , items :: [Item]
    } deriving (Show)

data Item = Item
    { itemId :: UUID
    , title :: Text
    , content :: Text
    } deriving (Show)

instance ToJSON Channel where
    toJSON Channel{..} = object [
        "id"    .= channelId
      , "name"  .= name
      , "items" .= items
      ]

instance ToJSON Item where
    toJSON Item{..} = object [
        "id"      .= itemId
      , "title"   .= title
      , "content" .= content
      ]

getChannels :: FilePath -> IO [Channel]
getChannels path = do
    feedFiles <- filter (\file -> takeExtension file == ".xml") <$> listDirectory path
    feeds <- mapM parseFeedFromFile feedFiles
    return $ map feedToChannel feeds

feedToChannel :: Feed -> Channel
feedToChannel (RSSFeed (RSS _ _ RSSChannel{..} _)) = Channel nil rssTitle (map itemToItem rssItems)
feedToChannel _ = error "Only RSSFeed supported"

itemToItem :: RSSItem -> Item
itemToItem RSSItem{..} =
    let title   = fromJust $ rssItemTitle <|> Just ""
        content = fromJust $ rssItemDescription <|> Just ""
    in Item { itemId = nil, title = title, content = content}
