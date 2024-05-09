{-# LANGUAGE OverloadedStrings #-}

module Frontmatter (
    Frontmatter (..),
    parseFrontmatter,
) where

import Data.Yaml (
    FromJSON (parseJSON),
    decodeFileEither,
    withObject,
    (.:),
 )

newtype Frontmatter = Frontmatter {title :: String} deriving (Show, Eq)

instance FromJSON Frontmatter where
    parseJSON = withObject "Frontmatter" $ \v ->
        Frontmatter
            <$> v .: "title"

parseFrontmatter :: FilePath -> IO (Maybe Frontmatter)
parseFrontmatter filePath = do
    result <- decodeFileEither filePath
    return $ case result of
        Left _ -> Nothing
        Right frontmatter -> Just frontmatter