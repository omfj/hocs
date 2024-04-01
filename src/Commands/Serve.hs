{-# LANGUAGE OverloadedStrings #-}

module Commands.Serve (
    runServeCommand,
) where

import Config (loadConfig)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.Lazy.IO as TIO
import FileUtils (getAllFiles)
import System.Directory (doesFileExist)
import System.FilePath (makeRelative, takeFileName)
import Web.Scotty (ScottyM, get, html, literal, scotty)

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
            else "/" ++ webPath

setupRoutes :: [FilePath] -> ScottyM ()
setupRoutes = mapM_ (createFileHandler "build")

runServeCommand :: IO ()
runServeCommand = do
    let port = 3000
    let hostname = "localhost"
    let buildDirectory = "build"

    loadConfig

    putStrLn "Serving the project"
    putStrLn $ "Listening on http://" ++ hostname ++ ":" ++ show port

    allFiles <- getAllFiles buildDirectory
    case length allFiles of
        0 -> putStrLn "No files found in build directory. Have you built the project?"
        _ -> scotty port (setupRoutes allFiles)
