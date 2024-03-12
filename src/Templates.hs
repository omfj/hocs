{-# LANGUAGE TemplateHaskell #-}

module Templates (
    gitIgnoreContents,
    hocsTomlContents,
    indexMdContents,
) where

import qualified Data.ByteString.UTF8 as BU
import Data.FileEmbed (embedFile)

gitIgnoreContents :: String
gitIgnoreContents = BU.toString $(embedFile "src/templates/gitignore")

hocsTomlContents :: String
hocsTomlContents = BU.toString $(embedFile "src/templates/hocs.toml")

indexMdContents :: String
indexMdContents = BU.toString $(embedFile "src/templates/index.md")
