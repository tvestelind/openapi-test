{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module SimpleRSS.ApiEndpoints
    ( runApp
    , Port
    ) where

import SimpleRSS.Feed

import Control.Lens hiding ((.=))
import Data.Map.Strict ((!?))
import Data.Maybe
import Data.Swagger
import Data.Text
import Data.UUID
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Swagger
import Servant.Swagger.UI
import Network.Wai.Middleware.Cors

type API =
         SwaggerSchemaUI "docs" "swagger.json"
    :<|> FeedsApi

type FeedsApi = "feeds" :>
    (    Get '[JSON] ChannelMap -- list feeds
    :<|> Capture "channelid" Text :> Get '[JSON] Channel -- view one feed
    )


server :: ChannelMap -> Server API
server map =
         swaggerSchemaUIServer swaggerDoc
    :<|> feeds
    :<|> oneFeed

    where feeds :: Handler ChannelMap
          feeds = return map

          oneFeed :: Text -> Handler Channel
          oneFeed uuidText = do
            uuid <- maybe (throwError err400 { errReasonPhrase = uuidError}) return (fromText uuidText)
            maybe (throwError err404) return (map !? uuid)
            where uuidError = unpack $ uuidText `append` " not a proper UUID"

swaggerDoc :: Swagger
swaggerDoc = toSwagger (Proxy :: Proxy FeedsApi)
    & info.title       .~ "RSS Dummy Backend"
    & info.version     .~ "1.0.0"
    & info.description ?~ "A dummy backend for playing RSS-ish data"


feedsAPI :: Proxy API
feedsAPI = Proxy

app :: ChannelMap -> Application
app map = simpleCors (serve feedsAPI (server map))

runApp :: Port -> ChannelMap -> IO ()
runApp port map = do
    putStrLn $ "server running on http://localhost:" ++ show port
    run port $ app map
