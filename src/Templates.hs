{-# LANGUAGE TemplateHaskell #-}

module Templates (
    gitIgnoreContents,
    hocsTomlContents,
    indexMdContents,
    styleCssContents,
    layoutHtmlContents,
    renderHTMLTemplate,
) where

import qualified Data.ByteString.UTF8 as BU
import Data.FileEmbed (embedFile)
import Data.List (isPrefixOf)

gitIgnoreContents :: String
gitIgnoreContents = BU.toString $(embedFile "src/templates/gitignore")

hocsTomlContents :: String
hocsTomlContents = BU.toString $(embedFile "src/templates/hocs.toml")

indexMdContents :: String
indexMdContents = BU.toString $(embedFile "src/templates/index.md")

styleCssContents :: String
styleCssContents = BU.toString $(embedFile "src/templates/css/style.css")

layoutHtmlContents :: String
layoutHtmlContents = BU.toString $(embedFile "src/templates/html/layout.html")

renderHTMLTemplate :: String -> [(String, String)] -> String
renderHTMLTemplate template [] = template
renderHTMLTemplate template ((key, value) : rest) = renderHTMLTemplate (replace ("{{" ++ key ++ "}}") value template) rest

replace :: String -> String -> String -> String
replace _ _ [] = []
replace from to str@(s : ss)
    | from `isPrefixOf` str = to ++ replace from to (drop (length from) str)
    | otherwise = s : replace from to ss
