{-# LANGUAGE OverloadedStrings #-}

module SimpleRSS.ApiEndpoints
    ( runApp
    , AppContex(..)
    , Port
    ) where

import SimpleRSS.Feed

import Control.Monad
import qualified Data.Map as Map
import Data.Maybe
import Data.UUID
import Network.HTTP.Types.Status
import Web.Spock
import Web.Spock.Action
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
        let maybeUuid = fromText id
        let uuid      = fromJust maybeUuid

        when (isNothing maybeUuid) $ do
            setStatus $ mkStatus 400 ""
            json $ show id ++ " is not a proper UUID"
        when (uuid `Map.notMember` channels) $ do
            setStatus $ mkStatus 404 ""
            json $ "couldn't find feed for UUID " ++ show uuid

        json (channels Map.! uuid)
