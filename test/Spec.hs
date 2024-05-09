{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Commands.Build (createHTMLFilename)
import qualified Data.Text as T
import NeatInterpolation (text)
import Test.Hspec (describe, hspec, it, shouldBe)
import Toml (Toml (..), TomlArray (..), TomlBool (TomlBool), TomlInteger (..), TomlString (..), TomlTable (..), TomlValue (..), parseToml)

main :: IO ()
main = hspec $ do
    describe "Commands.Build" $ do
        it "creates HTML filename" $ do
            createHTMLFilename "pages/index.md" `shouldBe` "index.html"
            createHTMLFilename "pages/about.md" `shouldBe` "about.html"
            createHTMLFilename "pages/subpage/contact.md" `shouldBe` "subpage/contact.html"
