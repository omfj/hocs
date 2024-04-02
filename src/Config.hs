{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Config (
    loadConfig,
    getPort,
    getBuildDirectory,
) where

import Toml (Toml (..), TomlInteger (..), TomlString (..), TomlTable (..), TomlValue (..), parseToml)

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

getPort :: Config -> Integer
getPort = port . build

getBuildDirectory :: Config -> String
getBuildDirectory = directory . build

configPath :: String
configPath = "./hocs.toml"

loadConfig :: IO (Maybe Config)
loadConfig = do
    contents <- readFile configPath
    return $ case parseToml contents of
        Left _ -> Nothing
        Right toml -> Just $ tomlToConfig toml

tomlToConfig :: Toml -> Config
tomlToConfig (Toml (TomlTable table)) =
    Config
        { name = name
        , theme = tomlToTheme themeTable
        , build = tomlToBuild buildTable
        }
  where
    themeTable = lookup "theme" table
    buildTable = lookup "build" table
    name = tomlToString $ lookup "name" table

tomlToTheme :: Maybe TomlValue -> Theme
tomlToTheme (Just (TomlTableValue (TomlTable table))) =
    Theme
        { title = tomlToString $ lookup "title" table
        , description = tomlToString $ lookup "description" table
        }
tomlToTheme _ = Theme "" ""

tomlToBuild :: Maybe TomlValue -> Build
tomlToBuild (Just (TomlTableValue (TomlTable table))) =
    Build
        { port = tomlToInteger $ lookup "port" table
        , directory = tomlToString $ lookup "directory" table
        }
tomlToBuild _ = Build 0 ""

tomlToString :: Maybe TomlValue -> String
tomlToString (Just (TomlStringValue (TomlString s))) = s
tomlToString _ = ""

tomlToInteger :: Maybe TomlValue -> Integer
tomlToInteger (Just (TomlIntegerValue (TomlInteger i))) = i
tomlToInteger _ = 0
