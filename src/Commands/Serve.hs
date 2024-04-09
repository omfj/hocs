{-# LANGUAGE OverloadedStrings #-}

module Commands.Serve (
    runServeCommand,
) where

import Config (getBuildDirectory, getPort, loadConfig)

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.Lazy as T
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (addBase, noDots, staticPolicy, (>->))
import System.Directory (doesDirectoryExist)
import Web.Scotty (
    captureParam,
    get,
    html,
    middleware,
    regex,
    scotty,
 )

runServeCommand :: IO ()
runServeCommand = do
    maybeConfig <- loadConfig
    case maybeConfig of
        Nothing -> putStrLn "No configuration file found"
        Just config -> do
            let port = fromIntegral $ getPort config
            let buildDirectory = getBuildDirectory config

            dirExists <- doesDirectoryExist buildDirectory
            if dirExists
                then do
                    putStrLn $ "Serving files from " ++ buildDirectory ++ " on port " ++ show port
                    serveFiles port buildDirectory
                else putStrLn "Build directory does not exist"

serveFiles :: Int -> String -> IO ()
serveFiles port basePath = do
    scotty port $ do
        middleware logStdoutDev
        middleware $ staticPolicy (noDots >-> addBase basePath)

        get "/" $ do
            file <- liftIO $ readFile $ basePath ++ "/index.html"
            html $ T.pack file

        get (regex ".*") $ do
            filename <- captureParam "0"
            file <- liftIO $ readFile $ basePath ++ "/" ++ filename ++ ".html"
            html $ T.pack file
