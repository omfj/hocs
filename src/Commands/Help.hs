module Commands.Help (
    runHelpCommand,
) where

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