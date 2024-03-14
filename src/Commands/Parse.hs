module Commands.Parse (
    parse,
) where

import Commands.Build (runBuildCommand)
import Commands.Help (runHelpCommand)
import Commands.Init (runInitCommand)
import Commands.Serve (runServeCommand)

parse :: [String] -> IO ()
parse ["init"] = runInitCommand
parse ["build"] = runBuildCommand
parse ["serve"] = runServeCommand
parse ["help"] = runHelpCommand
parse args = do
    if Prelude.null args
        then putStrLn "No command provided"
        else putStrLn $ "Unknown command: " ++ unwords args
    putStrLn ""
    runHelpCommand
