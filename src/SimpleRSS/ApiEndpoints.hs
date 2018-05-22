{-# LANGUAGE OverloadedStrings #-}

module SimpleRSS.ApiEndpoints
    ( runApp
    , AppContex(..)
    , Port
    ) where

import SimpleRSS.Feed

import Data.Map ((!))
import Data.Maybe
import Data.UUID
import Web.Spock
import Web.Spock.Config

type Port = Int
newtype AppContex = AppContex
    { channels :: ChannelMap
    }

runApp :: Port -> AppContex -> IO ()
runApp port context = do
    spockCfg <- defaultSpockCfg () PCNoDatabase context
    runSpock port (spock spockCfg app)

type Session = ()
type Database = ()

app :: SpockM Session Database AppContex ()
app = do
    get "feeds" $ do
        (AppContex channels) <- getState
        json channels
    get ("feeds" <//> var) $ \id -> do
        (AppContex channels) <- getState
        let uuid = fromJust $ fromText id
        json $ channels ! uuid
