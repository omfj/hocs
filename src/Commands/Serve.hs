{-# LANGUAGE OverloadedStrings #-}

module Commands.Serve (
    runServeCommand,
) where

import Config (getBuildDirectory, getPort, loadConfig)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.Lazy.IO as TIO
import FileUtils (getAllFiles)
import System.Directory (doesFileExist)
import System.FilePath (makeRelative, takeFileName)
import Web.Scotty (ScottyM, get, html, literal, scotty)

createFileHandler :: FilePath -> FilePath -> ScottyM ()
createFileHandler root filePath = get (literal routePath) $ do
    fileExists <- liftIO $ doesFileExist filePath
    if fileExists
        then do
            fileContents <- liftIO $ TIO.readFile filePath
            html fileContents
        else do
            html "<h1>404 - Not Found</h1>"
  where
    routePath = makeWebPath root filePath

makeWebPath :: FilePath -> FilePath -> String
makeWebPath root filePath = case webPath of
    "index" -> "/"
    _ -> "/" ++ webPath
  where
    relativePath = makeRelative root filePath
    fileName = takeFileName relativePath
    webPath = reverse $ drop 5 $ reverse fileName

setupRoutes :: String -> [FilePath] -> ScottyM ()
setupRoutes buildDir = mapM_ (createFileHandler buildDir)

runServeCommand :: IO ()
runServeCommand = do
    maybeConfig <- loadConfig
    case maybeConfig of
        Nothing -> putStrLn "No configuration file found"
        Just config -> do
            let port = fromIntegral $ getPort config
            let buildDirectory = getBuildDirectory config

            allFiles <- getAllFiles buildDirectory
            case allFiles of
                [] -> putStrLn $ "No files found in build directory ( " ++ buildDirectory ++ " ) . Have you built the project?"
                _ -> do
                    let hostname = "localhost"
                    putStrLn $ "Listening on http://" ++ hostname ++ ":" ++ show port
                    scotty port (setupRoutes buildDirectory allFiles)
