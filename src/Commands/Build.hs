{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Commands.Build (
    runBuildCommand,
    createHTMLFilename,
) where

import Config (getBuildDirectory, loadConfig)
import Control.Monad (when)
import Data.List (isSuffixOf)
import qualified Data.Text as T
import FileUtils (File (..), getAllFiles, writeFileAndDirectories)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, removeDirectoryRecursive)
import System.FilePath (replaceExtension)
import Templates (layoutHtmlContents, renderHTMLTemplate, styleCssContents)
import Text.Pandoc (def, readMarkdown, runIO, writeHtml5String)

-- ignoredStaticFiles :: [String]
-- ignoredStaticFiles = [".gitkeep"]

createHTMLFilename :: FilePath -> FilePath
createHTMLFilename = filename
  where
    filename = removeFirstFolder . replaceFileExtension
    removeFirstFolder = drop 1 . dropWhile (/= '/') . tail
    replaceFileExtension = flip replaceExtension "html"

markdownContents :: String -> IO String
markdownContents filePath = do
    contents <- readFile filePath
    result <- runIO $ do
        doc <- readMarkdown def (T.pack contents)
        writeHtml5String def doc
    case result of
        Left err -> return $ "Error: " ++ show err
        Right html -> do
            return $ renderHTMLTemplate layoutHtmlContents [("content", T.unpack html)]

createHTMLFile :: FilePath -> IO File
createHTMLFile filePath = do
    let fileName = createHTMLFilename filePath
    fileContents <- markdownContents filePath
    putStrLn $ "Creating file: " ++ fileName
    return $
        File
            { name = fileName
            , contents = fileContents
            }

createStaticFile :: FilePath -> IO File
createStaticFile filePath = do
    fileContents <- readFile filePath
    let fileName = removeFirstFolder filePath
    putStrLn $ "Creating file: " ++ fileName
    return $
        File
            { name = fileName
            , contents = fileContents
            }
  where
    removeFirstFolder = drop 1 . dropWhile (/= '/') . tail

isMarkdownFile :: String -> Bool
isMarkdownFile = isSuffixOf ".md"

removeDirectoryRecursiveIfExists :: FilePath -> IO ()
removeDirectoryRecursiveIfExists path = do
    exists <- doesDirectoryExist path
    when exists $ removeDirectoryRecursive path

getPagesFiles :: IO [File]
getPagesFiles = getAllFiles "pages" >>= mapM createHTMLFile . filter isMarkdownFile

getStaticFiles :: IO [File]
getStaticFiles = do
    let defaultStaticFiles = [File{name = "css/styles.css", contents = styleCssContents}]
    staticFiles <- getAllFiles "static" >>= mapM createStaticFile
    return $ defaultStaticFiles ++ staticFiles

createBuildDirectory :: FilePath -> [File] -> IO ()
createBuildDirectory buildDirectory files = do
    createDirectoryIfMissing True buildDirectory
    mapM_ (\file -> writeFileAndDirectories (buildDirectory ++ "/" ++ name file) (contents file)) files

runBuildCommand :: IO ()
runBuildCommand = do
    maybeConfig <- loadConfig
    case maybeConfig of
        Nothing -> putStrLn "No configuration file found"
        Just config -> do
            let buildDirectory = getBuildDirectory config

            -- Remove the build directory if it exists
            removeDirectoryRecursiveIfExists buildDirectory

            -- Get the path of all files in the pages directory
            allPagesFiles <- getPagesFiles
            allStaticFiles <- getStaticFiles

            let allFiles = allPagesFiles ++ allStaticFiles

            -- Create the build directory and write the contents of the files
            createBuildDirectory buildDirectory allFiles
