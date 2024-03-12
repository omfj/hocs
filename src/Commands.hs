{-# LANGUAGE OverloadedStrings #-}

module Commands (
    parse,
) where

import Control.Monad.IO.Class (liftIO)
import Data.List (isSuffixOf)
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TIO
import FileUtils (getAllFiles)
import System.Directory (createDirectory, createDirectoryIfMissing, doesFileExist)
import System.FilePath (makeRelative, takeDirectory, takeFileName)
import Templates (gitIgnoreContents, hocsTomlContents, indexMdContents)
import Text.Pandoc (runPure)
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Options (def)
import Text.Pandoc.Readers (readMarkdown)
import Text.Pandoc.Writers (writeHtml5String)
import Web.Scotty (ScottyM, get, html, literal, scotty)

parse :: [String] -> IO ()
parse ["init"] = runInitCommand
parse ["build"] = runBuildCommand
parse ["serve"] = runServeCommand
parse ["help"] = runHelpCommand
parse args = do
    if Prelude.null args
        then putStrLn "No command provided"
        else putStrLn $ "Unknown command: " ++ unwords args
    putStrLn ""
    runHelpCommand

runInitCommand :: IO ()
runInitCommand = do
    putStrLn "Initializing a new project"

    -- Create the .gitignore file
    writeFile ".gitignore" gitIgnoreContents
    putStrLn "Created .gitignore"

    -- Create the hocs.toml file
    writeFile "hocs.toml" hocsTomlContents
    putStrLn "Created hocs.toml"

    -- Create an empty pages directory
    createDirectory "pages"
    putStrLn "Created pages/"

    -- Create the index.md file
    writeFile "pages/index.md" indexMdContents
    putStrLn "Created pages/index.md"

    putStrLn "Project initialized"

data HTMLFileData = HTMLFileData
    { filename :: FilePath
    , contents :: String
    }
    deriving (Show)

convertToMarkdown :: String -> Either PandocError T.Text
convertToMarkdown markdownString =
    runPure $ do
        doc <- readMarkdown def (T.pack markdownString)
        writeHtml5String def doc

createHTMLFilename :: FilePath -> FilePath
createHTMLFilename name = "build/" ++ (drop 6 . (++ ".html") . reverse . drop 3 . reverse) name

createHTMLFileData :: FilePath -> Either PandocError T.Text -> HTMLFileData
createHTMLFileData name (Left _) = HTMLFileData (createHTMLFilename name) "<p>500</p>"
createHTMLFileData name (Right "") = HTMLFileData (createHTMLFilename name) "<p><i>Empty markdown</i></p>"
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

createFileHandler :: FilePath -> FilePath -> ScottyM ()
createFileHandler root filePath = do
    let routePath = makeWebPath root filePath
    get (literal routePath) $ do
        fileExists <- liftIO $ doesFileExist filePath
        if fileExists
            then do
                fileContents <- liftIO $ TIO.readFile filePath
                html fileContents
            else do
                html "<h1>404 - Not Found</h1>"

makeWebPath :: FilePath -> FilePath -> String
makeWebPath root filePath =
    let relativePath = makeRelative root filePath
        fileName = takeFileName relativePath
        webPath = reverse $ drop 5 $ reverse fileName
     in if webPath == "index"
            then "/"
            else "/" <> webPath

setupRoutes :: [FilePath] -> ScottyM ()
setupRoutes = mapM_ (createFileHandler "build")

runServeCommand :: IO ()
runServeCommand = do
    putStrLn "Serving the project"
    putStrLn "Listening on http://localhost:3000"

    let buildDirectory = "build"
    allFiles <- getAllFiles buildDirectory

    scotty 3000 (setupRoutes allFiles)

runHelpCommand :: IO ()
runHelpCommand =
    putStrLn $
        unlines
            [ "Command line usage for hocs"
            , ""
            , "hocs init -- Initialize a new project"
            , "hocs build -- Build the project"
            , "hocs serve -- Serve the project"
            ]