{-# LANGUAGE OverloadedStrings #-}

module SimpleRSS.Options
    ( Options(..)
    , getOptions
    ) where

import Options.Applicative
import Data.Semigroup ((<>))

data Options = Options
    { port :: Int
    , feedsdir :: FilePath
    }

getOptions :: IO Options
getOptions = execParser commandline

commandline :: ParserInfo Options
commandline = info (options <**> helper) $
       fullDesc
    <> progDesc "A simple RSS feed backend"
    <> header "Simple RSS feed backend"

options :: Parser Options
options = Options <$> port <*> feedsdir
    where
        port :: Parser Int
        port = option auto $
               long "port"
            <> short 'p'
            <> metavar "INT"
            <> value defaultPort
            <> help ("Port on which to run the web server. Default: " ++ show defaultPort)
            where defaultPort = 8080
        feedsdir :: Parser FilePath
        feedsdir = strOption $
               long "feedsdir"
            <> short 'd'
            <> metavar "DIRECTORY"
            <> value defaultDir
            <> help ("Directory from where to read the RSS feed XML files. Default: " ++ show defaultDir)
            where defaultDir = "./"
