{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import qualified Data.Text as T
import NeatInterpolation (text)
import Test.Hspec (describe, hspec, it, shouldBe)
import Toml (Toml (..), TomlArray (..), TomlBool (TomlBool), TomlInteger (..), TomlString (..), TomlTable (..), TomlValue (..), parseToml)

main :: IO ()
main = hspec $ do
    describe "Hocs.Toml" $ do
        it "parses key value pairs" $ do
            let toml =
                    T.unpack
                        [text|
                key = "value"
            |]

            parseToml toml
                `shouldBe` Right
                    ( Toml
                        ( TomlTable
                            [ ("key", TomlStringValue (TomlString "value"))
                            ]
                        )
                    )

        it "parses numbers" $ do
            let toml =
                    T.unpack
                        [text|
                key = 42
            |]

            parseToml toml
                `shouldBe` Right
                    ( Toml
                        ( TomlTable
                            [ ("key", TomlIntegerValue (TomlInteger 42))
                            ]
                        )
                    )

        it "parses booleans" $ do
            let toml =
                    T.unpack
                        [text|
                key = true
            |]

            parseToml toml
                `shouldBe` Right
                    ( Toml
                        ( TomlTable
                            [ ("key", TomlBoolValue (TomlBool True))
                            ]
                        )
                    )

        it "parses arrays" $ do
            let toml =
                    T.unpack
                        [text|
                key = [1, 2, 3]
            |]

            parseToml toml
                `shouldBe` Right
                    ( Toml
                        ( TomlTable
                            [
                                ( "key"
                                , TomlArrayValue
                                    ( TomlArray
                                        [ TomlIntegerValue (TomlInteger 1)
                                        , TomlIntegerValue (TomlInteger 2)
                                        , TomlIntegerValue (TomlInteger 3)
                                        ]
                                    )
                                )
                            ]
                        )
                    )

        it "parses nested tables" $ do
            let toml =
                    T.unpack
                        [text|
                [table]
                key = "value"
            |]

            parseToml toml
                `shouldBe` Right
                    ( Toml
                        ( TomlTable
                            [
                                ( "table"
                                , TomlTableValue
                                    ( TomlTable
                                        [ ("key", TomlStringValue (TomlString "value"))
                                        ]
                                    )
                                )
                            ]
                        )
                    )
        it "parses basic config file" $ do
            let toml =
                    T.unpack
                        [text|
                name = "hocs"

                [theme]
                title = "Documentation"
                description = "Documentation for the Hocs project"

                [build]
                port = 3000
            |]

            parseToml toml
                `shouldBe` Right
                    ( Toml
                        ( TomlTable
                            [
                                ( "build"
                                , TomlTableValue
                                    ( TomlTable
                                        [ ("port", TomlIntegerValue (TomlInteger 3000))
                                        ]
                                    )
                                )
                            ,
                                ( "theme"
                                , TomlTableValue
                                    ( TomlTable
                                        [ ("title", TomlStringValue (TomlString "Documentation"))
                                        , ("description", TomlStringValue (TomlString "Documentation for the Hocs project"))
                                        ]
                                    )
                                )
                            , ("name", TomlStringValue (TomlString "hocs"))
                            ]
                        )
                    )
