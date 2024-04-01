module Commands.Version (
    runVersionCommand,
) where

version :: String
version = "0.1.0"

runVersionCommand :: IO ()
runVersionCommand = putStrLn $ "Hocs version " ++ version