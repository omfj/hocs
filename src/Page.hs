{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Page (
    getAllPages,
    Page (..),
    dispalyPageFilepath,
    displayPageContent,
    displayPageTitle,
    getSidebarItems,
    renderSidebarItems,
    createFilename,
) where

import Data.List (isPrefixOf, isSuffixOf)
import qualified Data.Text as T
import FileUtils (getAllFiles, isMarkdownFile, removeFirstFolder)
import Frontmatter (Frontmatter (..), parseFrontmatter)
import System.FilePath (replaceExtension)
import Text.Pandoc (def, readMarkdown, runIO, writeHtml5String)

data Page = Page
    { title :: String
    , filepath :: String
    , content :: String
    }

data SidebarItem = SidebarItem
    { title :: String
    , url :: String
    }

displayPageTitle :: Page -> String
displayPageTitle Page{title = title} = title

displayPageContent :: Page -> String
displayPageContent Page{content = content} = content

dispalyPageFilepath :: Page -> String
dispalyPageFilepath Page{filepath = filepath} = filepath

pagesDirectory :: String
pagesDirectory = "pages"

getAllPages :: IO [Page]
getAllPages = do
    files <- getAllFiles pagesDirectory
    let markdownFiles = filter isMarkdownFile files
    mapM createPage markdownFiles

getSidebarItems :: IO [SidebarItem]
getSidebarItems = do
    map pageToSidebarItem <$> getAllPages
  where
    pageToSidebarItem page =
        SidebarItem
            { title = displayPageTitle page
            , url = createURLPath $ dispalyPageFilepath page
            }

renderSidebarItems :: [SidebarItem] -> String
renderSidebarItems sidebarItems = unlines $ map renderSidebarItem sidebarItems
  where
    renderSidebarItem :: SidebarItem -> String
    renderSidebarItem SidebarItem{title = title, url = url} =
        "<li><a href=\"/" ++ url ++ "\">" ++ title ++ "</a></li>"

createPage :: FilePath -> IO Page
createPage filePath = do
    (title, contents) <- readMarkdownFile filePath
    return $
        Page
            { title = title
            , filepath = filePath
            , content = contents
            }

readMarkdownFile :: FilePath -> IO (String, String)
readMarkdownFile filePath = do
    contents <- readFile filePath
    let (yaml, markdown) = splitYAML contents
    html <- markdownToHTML markdown
    title <- getTitle yaml markdown
    return (title, html)

-- Get the title from the YAML front matter or the first line of the markdown
getTitle :: String -> String -> IO String
getTitle yaml markdown = do
    frontmatter <- parseFrontmatter yaml
    case frontmatter of
        Just fm -> return $ dispalyFrontmatterTitle fm
        Nothing -> return $ getTitleFromMarkdown markdown

dispalyFrontmatterTitle :: Frontmatter -> String
dispalyFrontmatterTitle Frontmatter{title = title} = title

getTitleFromMarkdown :: String -> String
getTitleFromMarkdown markdown = do
    let headers = filter isHeader (lines markdown)
    case headers of
        [] -> ""
        (header : _) -> removeMarkup header
  where
    isHeader = isPrefixOf "#"
    removeMarkup = dropWhile (== '#')

splitYAML :: String -> (String, String)
splitYAML contents = ("", contents)
splitYAML contents = case splitYAML' contents of
    Just (yaml, markdown) -> (yaml, markdown)
    Nothing -> ("", contents)
  where
    splitYAML' :: String -> Maybe (String, String)
    splitYAML' contents = do
        let (yaml, rest) = breakOn "---" contents
        let (_, markdown) = breakOn "---" rest
        return (yaml, markdown)

breakOn :: String -> String -> (String, String)
breakOn pattern str = breakOn' pattern str ""
  where
    breakOn' :: String -> String -> String -> (String, String)
    breakOn' _ [] _ = ("", "")
    breakOn' pattern str acc
        | pattern `isPrefixOf` str = (acc, drop (length pattern) str)
        | otherwise = breakOn' pattern (tail str) (acc ++ [head str])

markdownToHTML :: String -> IO String
markdownToHTML content = do
    result <- runIO $ do
        doc <- readMarkdown def (T.pack content)
        writeHtml5String def doc

    case result of
        Left err -> return $ "Error: " ++ show err
        Right html -> return $ T.unpack html

createURLPath :: FilePath -> String
createURLPath = urlpath
  where
    urlpath = removeIndex . removeFirstFolder . replaceFileExtension
    replaceFileExtension = flip replaceExtension ""
    removeIndex path = if "index" `isSuffixOf` path then reverse $ drop 5 $ reverse path else path

createFilename :: FilePath -> FilePath
createFilename = filename
  where
    filename = removeFirstFolder . replaceFileExtension
    replaceFileExtension = flip replaceExtension "html"