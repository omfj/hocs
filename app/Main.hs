module Main (main) where

import Commands (parse)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    parse args