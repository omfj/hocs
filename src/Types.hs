module Types (
    HTMLFileData (..),
) where

data HTMLFileData = HTMLFileData
    { filename :: FilePath
    , contents :: String
    }
    deriving (Show)
