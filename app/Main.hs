module Main where

import SimpleRSS.Options
import SimpleRSS.ApiEndpoints
import SimpleRSS.Feed

main :: IO ()
main = do
    (Options port feedsdir) <- getOptions
    channels <- getChannels feedsdir
    runApp port (AppContex channels)
