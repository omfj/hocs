module Commands.Build (
    runBuildCommand,
) where

import Data.List (isSuffixOf)
import qualified Data.Text as T
import FileUtils (getAllFiles)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Text.Pandoc (PandocError, def, readMarkdown, runPure, writeHtml5String)
import Types (HTMLFileData (..))

convertToMarkdown :: String -> Either PandocError T.Text
convertToMarkdown markdownString =
    runPure $ do
        doc <- readMarkdown def (T.pack markdownString)
        writeHtml5String def doc

createHTMLFilename :: FilePath -> FilePath
createHTMLFilename name = "build/" ++ (drop 6 . (++ ".html") . reverse . drop 3 . reverse) name

createHTMLFileData :: FilePath -> Either PandocError T.Text -> HTMLFileData
createHTMLFileData name (Left _) = HTMLFileData (createHTMLFilename name) "<p>500</p>"
createHTMLFileData name (Right htmlText) = HTMLFileData (createHTMLFilename name) (T.unpack htmlText)

createBuildDirectory :: [HTMLFileData] -> IO ()
createBuildDirectory = mapM_ createHTMLFile

createFileAndFolder :: FilePath -> String -> IO ()
createFileAndFolder filePath fileContent = do
    let directory = takeDirectory filePath
    createDirectoryIfMissing True directory
    writeFile filePath fileContent

createHTMLFile :: HTMLFileData -> IO ()
createHTMLFile htmlFile = createFileAndFolder (filename htmlFile) (contents htmlFile)

runBuildCommand :: IO ()
runBuildCommand = do
    -- Get the path of all files in the pages directory
    allFiles <- getAllFiles "pages"
    let allMarkdownFiles = filter (".md" `isSuffixOf`) allFiles
    allMarkdownContents <- mapM readFile allMarkdownFiles

    -- Convert Markdown to HTML and create HTMLFileData instances
    let htmlContents = map convertToMarkdown allMarkdownContents
    let htmlFiles = zipWith createHTMLFileData allMarkdownFiles htmlContents

    -- Print HTMLFileData instances
    createBuildDirectory htmlFiles
