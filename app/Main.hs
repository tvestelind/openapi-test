{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Web.Spock
import Web.Spock.Config

import Data.Maybe
import Control.Applicative
import Data.Aeson hiding (json)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Data.UUID
import Text.Feed.Import
import qualified Text.Feed.Types as FeedTypes
import qualified Text.RSS.Syntax as FeedRSS

data RSSChannel = RSSChannel
    { channelId :: UUID
    , name :: Text
    , items :: [RSSItem]
    } deriving (Show)

data RSSItem = RSSItem
    { itemId :: UUID
    , title :: Text
    , content :: Text
    } deriving (Show)

instance ToJSON RSSChannel where
    toJSON RSSChannel{..} = object [
        "id"    .= channelId
      , "name"  .= name
      , "items" .= items
      ]

instance ToJSON RSSItem where
    toJSON RSSItem{..} = object [
        "id"      .= itemId
      , "title"   .= title
      , "content" .= content
      ]


newtype ApiContext = ApiContext
    { channels :: [RSSChannel]
    }

type Api = SpockM () () ApiContext ()

main :: IO ()
main = do
    feed <- parseFeedFromFile "sample.xml"
    print $ show $ feedToChannel feed
    spockCfg <- defaultSpockCfg () PCNoDatabase (ApiContext [feedToChannel feed])
    runSpock 8080 (spock spockCfg app)

feedToChannel :: FeedTypes.Feed -> RSSChannel
feedToChannel (FeedTypes.RSSFeed (FeedRSS.RSS _ _ FeedRSS.RSSChannel{..} _)) = RSSChannel nil rssTitle (map itemToItem rssItems)
feedToChannel _ = error "Only RSSFeed supported"

itemToItem :: FeedRSS.RSSItem -> RSSItem
itemToItem FeedRSS.RSSItem{..} =
    let title   = fromJust $ rssItemTitle <|> Just ""
        content = fromJust $ rssItemDescription <|> Just ""
    in RSSItem { itemId = nil, title = title, content = content}


app :: Api
app = do
    get "feeds" $ do
        (ApiContext channels) <- getState
        json channels
    get ("feeds" <//> var) $ \id -> json RSSChannel { channelId = id, name = "Kul", items = [] }
