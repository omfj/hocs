module FileUtils (
    getAllFiles,
) where

import Control.Monad (filterM, forM)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))

getAllFiles :: FilePath -> IO [FilePath]
getAllFiles dir = do
    -- Get all entries in the directory
    entries <- listDirectory dir
    -- Prepend the directory path to get absolute paths
    let fullPaths = map (dir </>) entries
    -- Separate the entries into files and directories
    files <- filterM doesFileExist fullPaths
    dirs <- filterM doesDirectoryExist fullPaths
    -- Recursively list files in all directories
    filesInSubdirs <- concat <$> forM dirs getAllFiles
    -- Return the list of files, including those in subdirectories
    return $ files ++ filesInSubdirs