{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Web.Spock
import Web.Spock.Config

import Data.Aeson hiding (json)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Data.UUID

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


type Api = SpockM () () () ()
type ApiAction a = SpockAction () () () a

main :: IO ()
main = do
    spockCfg <- defaultSpockCfg () PCNoDatabase ()
    runSpock 8080 (spock spockCfg app)

app :: Api
app = do
    get "feeds" $ json [
            RSSChannel { channelId = nil, name = "Min blag", items = [
                  RSSItem { itemId = nil, title = "Min title", content = "Lite stuff" }
                ]
            }
          , RSSChannel { channelId = nil, name = "Min blag 1", items = [
                  RSSItem { itemId = nil, title = "Min title 1", content = "Lite stuff 1" }
                , RSSItem { itemId = nil, title = "Min title 2", content = "Lite stuff 2" }
                ]
            }
          ]
    get ("feeds" <//> var) $ \id -> json RSSChannel { channelId = id, name = "Kul", items = [] }
