module Commands.Parse (
    parse,
) where

import Commands.Build (runBuildCommand)
import Commands.Help (runHelpCommand)
import Commands.Init (runInitCommand)
import Commands.Serve (runServeCommand)
import Commands.Version (runVersionCommand)

parse :: [String] -> IO ()
parse ["init"] = runInitCommand
parse ["build"] = runBuildCommand
parse ["serve"] = runServeCommand
parse ["help"] = runHelpCommand
parse ["version"] = runVersionCommand
parse ["-v"] = runVersionCommand
parse ["--help"] = runHelpCommand
parse ["-h"] = runHelpCommand
parse ["--version"] = runVersionCommand
parse args = do
    if null args
        then putStrLn "No command provided"
        else putStrLn $ "Unknown command: " ++ unwords args
    putStrLn ""
    runHelpCommand
