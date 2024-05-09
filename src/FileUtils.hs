module FileUtils (
    File (..),
    getAllFiles,
    writeFileAndDirectories,
    isMarkdownFile,
    removeDirectoryRecursiveIfExists,
    removeFirstFolder,
) where

import Control.Monad (filterM, forM, when)
import Data.List (isSuffixOf)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory, removeDirectoryRecursive)
import System.FilePath (takeDirectory, (</>))

data File = File
    { name :: FilePath
    , contents :: String
    }
    deriving (Eq, Show)

getAllFiles :: FilePath -> IO [FilePath]
getAllFiles dir = do
    dirExists <- doesDirectoryExist dir
    if dirExists
        then
            ( do
                entries <- listDirectory dir
                let fullPaths = map (dir </>) entries
                files <- filterM doesFileExist fullPaths
                dirs <- filterM doesDirectoryExist fullPaths
                filesInSubdirs <- concat <$> forM dirs getAllFiles
                return $ files ++ filesInSubdirs
            )
        else return []

writeFileAndDirectories :: FilePath -> String -> IO ()
writeFileAndDirectories filePath fileContents = do
    let directory = takeDirectory filePath
    createDirectoryIfMissing True directory
    writeFile filePath fileContents

isMarkdownFile :: String -> Bool
isMarkdownFile = isSuffixOf ".md"

removeDirectoryRecursiveIfExists :: FilePath -> IO ()
removeDirectoryRecursiveIfExists path = do
    exists <- doesDirectoryExist path
    when exists $ removeDirectoryRecursive path

removeFirstFolder :: String -> String
removeFirstFolder = drop 1 . dropWhile (/= '/') . tail
