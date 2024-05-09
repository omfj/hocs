{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Config (
    loadConfig,
    getPort,
    getBuildDirectory,
    getTitle,
) where

import Control.Exception (throwIO)
import Data.Aeson (FromJSON, parseJSON, withObject, (.:))
import Data.Yaml (decodeFileEither)

data Theme = Theme
    { title :: String
    , description :: String
    }
    deriving (Eq, Show)

data Build = Build
    { port :: Integer
    , directory :: String
    }
    deriving (Eq, Show)

data Config = Config
    { name :: String
    , theme :: Theme
    , build :: Build
    }
    deriving (Eq, Show)

getTitle :: Config -> String
getTitle = title . theme

getPort :: Config -> Integer
getPort = port . build

getBuildDirectory :: Config -> String
getBuildDirectory = directory . build

configPath :: String
configPath = "./hocs.yaml"

instance FromJSON Theme where
    parseJSON = withObject "Theme" $ \v ->
        Theme
            <$> v .: "title"
            <*> v .: "description"

instance FromJSON Build where
    parseJSON = withObject "Build" $ \v ->
        Build
            <$> v .: "port"
            <*> v .: "directory"

instance FromJSON Config where
    parseJSON = withObject "Config" $ \v ->
        Config
            <$> v .: "name"
            <*> v .: "theme"
            <*> v .: "build"

loadConfig :: IO Config
loadConfig = do
    result <- decodeFileEither configPath
    case result of
        Left err -> throwIO err
        Right config -> return config
