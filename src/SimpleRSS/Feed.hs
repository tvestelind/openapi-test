{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module SimpleRSS.Feed
    ( getChannels
    , ChannelMap
    ) where

import Control.Applicative
import Data.Aeson hiding (json)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Text (Text, pack)
import Data.UUID
import Data.UUID.V4
import GHC.Generics
import System.Directory
import System.FilePath
import Text.Feed.Import
import Text.Feed.Types hiding (RSSItem, Item)
import Text.RSS.Syntax

data Channel = Channel
    { name :: Text
    , items :: [Item]
    } deriving (Generic, Show)

data Item = Item
    { title :: Text
    , content :: Text
    } deriving (Generic, Show)

instance ToJSON Channel
instance ToJSON Item

type ChannelMap = Map.Map UUID Channel

getChannels :: FilePath -> IO ChannelMap
getChannels path = do
    feedFiles <- filter (\file -> takeExtension file == ".xml") <$> listDirectory path
    feeds <- mapM parseFeedFromFile feedFiles
    let channels = map feedToChannel feeds
    uuidFeedPairs <- mapM (\c -> do
            uuid <- nextRandom
            return (uuid, c)
        ) channels

    return $ Map.fromList uuidFeedPairs

feedToChannel :: Feed -> Channel
feedToChannel (RSSFeed (RSS _ _ RSSChannel{..} _)) = Channel rssTitle (map itemToItem rssItems)
feedToChannel _ = error "Only RSSFeed supported"

itemToItem :: RSSItem -> Item
itemToItem RSSItem{..} =
    let title   = fromJust $ rssItemTitle <|> Just ""
        content = fromJust $ rssItemDescription <|> Just ""
    in Item { title = title, content = content}
