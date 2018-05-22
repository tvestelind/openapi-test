{-# LANGUAGE OverloadedStrings #-}

module SimpleRSS.ApiEndpoints
    ( runApp
    , AppContex(..)
    , Port
    ) where

import SimpleRSS.Feed

import Web.Spock
import Web.Spock.Config

type Port = Int
newtype AppContex = AppContex
    { channels :: [Channel]
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
        json $ channels !! id
