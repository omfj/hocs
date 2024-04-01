module Commands.Init (
    runInitCommand,
) where

import System.Directory (createDirectory)
import Templates (gitIgnoreContents, hocsTomlContents, indexMdContents)

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

    -- Create a static directory with a .gitkeep file
    createDirectory "static"
    writeFile "static/.gitkeep" ""
    putStrLn "Created static/"

    putStrLn "Project initialized"