module Commands.Build (
    runBuildCommand,
) where

import Config (getBuildDirectory, loadConfig)
import Data.List (isSuffixOf)
import qualified Data.Text as T
import FileUtils (getAllFiles)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Text.Pandoc (PandocError, def, readMarkdown, runPure, writeHtml5String)
import Types (HTMLFileData (..))

convertToMarkdown :: String -> Either PandocError T.Text
convertToMarkdown markdownString = runPure $ do
    doc <- readMarkdown def (T.pack markdownString)
    writeHtml5String def doc

createHTMLFilename :: FilePath -> FilePath
createHTMLFilename = drop 6 . (++ ".html") . reverse . drop 3 . reverse

createHTMLFileData :: FilePath -> Either PandocError T.Text -> HTMLFileData
createHTMLFileData name (Left _) = HTMLFileData (createHTMLFilename name) "<p>500</p>"
createHTMLFileData name (Right htmlText) = HTMLFileData (createHTMLFilename name) (T.unpack htmlText)

createBuildDirectory :: [HTMLFileData] -> IO ()
createBuildDirectory = mapM_ createHTMLFile

createFileAndFolder :: FilePath -> String -> IO ()
createFileAndFolder filePath fileContent = do
    createDirectoryIfMissing True directory
    writeFile filePath fileContent
  where
    directory = takeDirectory filePath

createHTMLFile :: HTMLFileData -> IO ()
createHTMLFile htmlFile = createFileAndFolder (filename htmlFile) (contents htmlFile)

runBuildCommand :: IO ()
runBuildCommand = do
    maybeConfig <- loadConfig
    case maybeConfig of
        Nothing -> putStrLn "No configuration file found"
        Just config -> do
            let buildDirectory = getBuildDirectory config
            -- Get the path of all files in the pages directory
            allFiles <- getAllFiles "pages"
            let allMarkdownFiles = filter (".md" `isSuffixOf`) allFiles
            allMarkdownContents <- mapM readFile allMarkdownFiles

            -- Convert Markdown to HTML and create HTMLFileData instances
            let htmlContents = map convertToMarkdown allMarkdownContents
            let htmlFiles = zipWith createHTMLFileData allMarkdownFiles htmlContents

            -- Prefix all filenames with build directory
            let htmlFilesWithBuildDirectory = map (\htmlFile -> htmlFile{filename = buildDirectory ++ "/" ++ filename htmlFile}) htmlFiles

            -- Print HTMLFileData instances
            createBuildDirectory htmlFilesWithBuildDirectory
