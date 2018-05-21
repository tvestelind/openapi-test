{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Web.Spock
import Web.Spock.Config

import Data.Maybe
import Control.Applicative
import Data.Aeson hiding (json)
import Data.Text (Text, pack)
import Data.UUID
import Text.Feed.Import
import qualified Text.Feed.Types as FeedTypes
import qualified Text.RSS.Syntax as FeedRSS
import Data.Semigroup ((<>))
import qualified Options.Applicative as Opt

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

data Opts = Opts
    { port :: Int
    , feedsdir :: FilePath
    }

main :: IO ()
main = do
    feed <- parseFeedFromFile "sample.xml"
    spockCfg <- defaultSpockCfg () PCNoDatabase (ApiContext [feedToChannel feed])
    (Opts port feedsdir) <- Opt.execParser commandline
    runSpock port (spock spockCfg app)

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

commandline :: Opt.ParserInfo Opts
commandline = Opt.info (options <**> Opt.helper) $
       Opt.fullDesc
    <> Opt.progDesc "A simple RSS feed backend"
    <> Opt.header "Simple RSS feed backend"

options :: Opt.Parser Opts
options = Opts <$> port <*> feedsdir
    where
        port :: Opt.Parser Int
        port = Opt.option Opt.auto $
               Opt.long "port"
            <> Opt.short 'p'
            <> Opt.metavar "INT"
            <> Opt.value defaultPort
            <> Opt.help ("Port on which to run the web server. Default: " ++ show defaultPort)
            where defaultPort = 8080
        feedsdir :: Opt.Parser FilePath
        feedsdir = Opt.strOption $
               Opt.long "feedsdir"
            <> Opt.short 'd'
            <> Opt.metavar "DIRECTORY"
            <> Opt.value defaultDir
            <> Opt.help ("Directory from where to read the RSS feed XML files. Default: " ++ show defaultDir)
            where defaultDir = "./"
