{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module SimpleRSS.ApiEndpoints
    ( runApp
    , Port
    ) where

import SimpleRSS.Feed

import Data.UUID
import Data.Map.Strict ((!?))
import Data.Maybe
import Network.Wai
import Network.Wai.Handler.Warp
import Servant


type FeedsApi = "feeds" :>
    (    Get '[JSON] ChannelMap -- list feeds
    :<|> Capture "channelid" UUID :> Get '[JSON] Channel -- view one feed
    )


server :: ChannelMap -> Server FeedsApi
server map = feeds
    :<|> oneFeed

    where feeds :: Handler ChannelMap
          feeds = return map

          oneFeed :: UUID -> Handler Channel
          oneFeed uuid = maybe (throwError err404) return (map !? uuid)

feedsAPI :: Proxy FeedsApi
feedsAPI = Proxy

app :: ChannelMap -> Application
app map = serve feedsAPI (server map)

runApp :: Port -> ChannelMap -> IO ()
runApp port map = run port $ app map
