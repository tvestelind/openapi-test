module Main where

import SimpleRSS.Options
import SimpleRSS.ApiEndpoints
import SimpleRSS.Feed

main :: IO ()
main = do
    (Options port feedsdir) <- getOptions
    channelMap <- getChannels feedsdir
    runApp port (AppContex channelMap)
