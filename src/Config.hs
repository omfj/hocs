{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Config (
    loadConfig,
) where

import Toml (Toml (..), TomlInteger (..), TomlString (..), TomlTable (..), TomlValue (..), parseToml)

data Theme = Theme
    { title :: String
    , description :: String
    }
    deriving (Eq, Show)

data Build = Build
    { port :: Integer
    }
    deriving (Eq, Show)

data Config = Config
    { name :: String
    , theme :: Theme
    , build :: Build
    }
    deriving (Eq, Show)

configPath :: String
configPath = "./hocs.toml"

loadConfig :: IO ()
loadConfig = do
    contents <- readFile configPath
    case parseToml contents of
        Left err -> print err
        Right toml -> do
            let config = tomlToConfig toml
            print config

tomlToConfig :: Toml -> Config
tomlToConfig (Toml (TomlTable table)) =
    Config
        { name = "hocs"
        , theme = tomlToTheme themeTable
        , build = tomlToBuild buildTable
        }
  where
    themeTable = lookup "theme" table
    buildTable = lookup "build" table

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
        }
tomlToBuild _ = Build 0

tomlToString :: Maybe TomlValue -> String
tomlToString (Just (TomlStringValue (TomlString s))) = s
tomlToString _ = ""

tomlToInteger :: Maybe TomlValue -> Integer
tomlToInteger (Just (TomlIntegerValue (TomlInteger i))) = i
tomlToInteger _ = 0
