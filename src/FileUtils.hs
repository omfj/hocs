module FileUtils (
    getAllFiles,
) where

import Control.Monad (filterM, forM)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))

getAllFiles :: FilePath -> IO [FilePath]
getAllFiles dir = do
    entries <- listDirectory dir

    let fullPaths = map (dir </>) entries

    files <- filterM doesFileExist fullPaths
    dirs <- filterM doesDirectoryExist fullPaths
    filesInSubdirs <- concat <$> forM dirs getAllFiles

    return $ files ++ filesInSubdirs