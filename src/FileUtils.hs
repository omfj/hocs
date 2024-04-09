module FileUtils (
    File (..),
    getAllFiles,
    writeFileAndDirectories,
) where

import Control.Monad (filterM, forM)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeDirectory, (</>))

data File = File
    { name :: FilePath
    , contents :: String
    }
    deriving (Eq, Show)

getAllFiles :: FilePath -> IO [FilePath]
getAllFiles dir = do
    dirExists <- doesDirectoryExist dir
    if not dirExists
        then return []
        else do
            entries <- listDirectory dir

            let fullPaths = map (dir </>) entries

            files <- filterM doesFileExist fullPaths
            dirs <- filterM doesDirectoryExist fullPaths
            filesInSubdirs <- concat <$> forM dirs getAllFiles

            return $ files ++ filesInSubdirs

writeFileAndDirectories :: FilePath -> String -> IO ()
writeFileAndDirectories filePath fileContents = do
    let directory = takeDirectory filePath
    createDirectoryIfMissing True directory
    writeFile filePath fileContents
