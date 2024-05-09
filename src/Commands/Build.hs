{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Commands.Build (
  runBuildCommand,
) where

import Config (getBuildDirectory, getTitle, loadConfig)
import Control.Monad (forM_)
import Data.Functor ((<&>))
import FileUtils (getAllFiles, removeDirectoryRecursiveIfExists, removeFirstFolder, writeFileAndDirectories)
import Page (Page (..), createFilename, getAllPages, getSidebarItems, renderSidebarItems)
import System.Directory (createDirectoryIfMissing)
import Templates (layoutHTMLContents, renderHTMLTemplate, styleCssContents, variablesCssContents)

data File = File
  { filename :: String
  , filecontent :: String
  }

getAllStaticFiles :: IO [FilePath]
getAllStaticFiles = getAllFiles "static" <&> filter isIgnored
 where
  isIgnored = elem `flip` [".", "..", ".DS_Store", ".gitkeep"]

pageToGenericFile :: String -> String -> Page -> IO File
pageToGenericFile title buildDir page = do
  let htmlFilename = buildDir ++ "/" ++ createFilename (filepath page)

  sidebarItems <- getSidebarItems

  let fullHTMLContent =
        renderHTMLTemplate
          layoutHTMLContents
          [ ("content", content page)
          , ("sidebarItems", renderSidebarItems sidebarItems)
          , ("title", title)
          ]

  return $ File{filename = htmlFilename, filecontent = fullHTMLContent}

staticFileToGenericFile :: String -> FilePath -> IO File
staticFileToGenericFile buildDir filePath = do
  contents <- readFile filePath
  return $ File{filename = filename filePath, filecontent = contents}
 where
  filename = (++ buildDir ++ "/") . removeFirstFolder

getDefaultStaticFiles :: String -> IO [File]
getDefaultStaticFiles buildDir = do
  return $ map (\file -> File{filename = buildDir ++ "/" ++ filename file, filecontent = filecontent file}) defaultStaticFiles
 where
  defaultStaticFiles =
    [ File{filename = "css/styles.css", filecontent = styleCssContents}
    , File{filename = "css/variables.css", filecontent = variablesCssContents}
    ]

runBuildCommand :: IO ()
runBuildCommand = do
  config <- loadConfig
  let buildDirectory = getBuildDirectory config
  let title = getTitle config

  removeDirectoryRecursiveIfExists buildDirectory
  createDirectoryIfMissing True buildDirectory

  staticFiles <- getAllStaticFiles
  pagesFiles <- getAllPages
  defaultStaticFiles <- getDefaultStaticFiles buildDirectory

  staticGenericFiles <- mapM (staticFileToGenericFile buildDirectory) staticFiles
  pagesGenericFiles <- mapM (pageToGenericFile title buildDirectory) pagesFiles

  forM_ (staticGenericFiles ++ pagesGenericFiles ++ defaultStaticFiles) $ \file -> do
    writeFileAndDirectories (filename file) (filecontent file)
    putStrLn $ "Created " ++ filename file
