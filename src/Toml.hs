{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Toml (
    Toml (..),
    TomlValue (..),
    TomlInteger (..),
    TomlFloat (..),
    TomlString (..),
    TomlBool (..),
    TomlArray (..),
    TomlTable (..),
    TomlError (..),
    parseToml,
) where

import Data.Char (isDigit)

data TomlInteger
    = TomlInteger Integer
    deriving (Show, Eq)

data TomlFloat
    = TomlFloat Double
    deriving (Show, Eq)

data TomlString
    = TomlString String
    deriving (Show, Eq)

data TomlBool
    = TomlBool Bool
    deriving (Show, Eq)

data TomlArray
    = TomlArray [TomlValue]
    deriving (Show, Eq)

data TomlTable
    = TomlTable [(String, TomlValue)]
    deriving (Show, Eq)

data TomlValue
    = TomlIntegerValue TomlInteger
    | TomlFloatValue TomlFloat
    | TomlStringValue TomlString
    | TomlBoolValue TomlBool
    | TomlArrayValue TomlArray
    | TomlTableValue TomlTable
    deriving (Show, Eq)

data Toml
    = Toml TomlTable
    deriving (Show, Eq)

data TomlError
    = TomlParseError String
    deriving (Show, Eq)

parseToml :: String -> Either TomlError Toml
parseToml contents = Right . Toml . TomlTable $ parseLines $ filter (\l -> trim l /= "") (lines contents)

parseLines :: [String] -> [(String, TomlValue)]
parseLines l = parseLines' l []

parseLines' :: [String] -> [(String, TomlValue)] -> [(String, TomlValue)]
parseLines' [] acc = acc
parseLines' (l : ls) acc
    | isTableHeader l = do
        let (tableName, restLines) = break isTableHeader ls
        let tableContents = parseLines' tableName []
        parseLines' restLines ((trim $ tableHeaderName l, TomlTableValue (TomlTable tableContents)) : acc)
    | otherwise = parseLines' ls (acc ++ [(key, parseValue value)])
  where
    (key, value) = parseKeyValue l

isTableHeader :: String -> Bool
isTableHeader l = case l of
    '[' : xs -> last xs == ']'
    _ -> False

tableHeaderName :: String -> String
tableHeaderName = init . tail

parseKeyValue :: String -> (String, String)
parseKeyValue l =
    let key = trim $ takeWhile (/= '=') l
        value = trim $ drop 1 $ dropWhile (/= '=') l
     in (key, value)

parseValue :: String -> TomlValue
parseValue value
    --  If it's a string we know it has at least 1 character, so we can safely use init and tail
    | isString value = TomlStringValue (TomlString (init (tail value)))
    | isInteger value = TomlIntegerValue (TomlInteger (read value))
    | isTrue value = TomlBoolValue (TomlBool True)
    | isFalse value = TomlBoolValue (TomlBool False)
    -- If it's an array we know it has at least 2 characters, so we can safely use init and tail
    | isArray value = TomlArrayValue (TomlArray (parseArray (init (tail value))))
    | otherwise = error "Unsupported value type"

parseArray :: String -> [TomlValue]
parseArray "" = []
parseArray contents =
    let (val, rest) = break (== ',') contents
     in case trim val of
            "" -> []
            trimmedVal -> parseValue trimmedVal : parseArray (trim $ drop 1 rest)

isArray :: String -> Bool
isArray s = case s of
    '[' : xs -> last xs == ']'
    _ -> False

isString :: String -> Bool
isString s = case s of
    '"' : xs -> last xs == '"'
    _ -> False

isTrue :: String -> Bool
isTrue s = s == "true"

isFalse :: String -> Bool
isFalse s = s == "false"

isInteger :: String -> Bool
isInteger = all isDigit

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile (== ' ')
