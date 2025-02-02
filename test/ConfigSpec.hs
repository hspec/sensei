{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
module ConfigSpec (spec) where

import Helper
import Data.Aeson
import Data.Yaml.TH
import Data.ByteString.Lazy qualified as LB

import Config
import Config.DeepSeek

spec :: Spec
spec = do
  describe "readConfigFilesFrom" $ do
    it "reads config values from ./sensei.yaml" $ do
      withTempDirectory $ \ dir -> do
        let
          global = dir </> "global.yaml"
          local = dir </> "local.yaml"
        LB.writeFile local $ encode [yamlQQ|
          before-reload: local-before-reload
          on-success: local-on-success
        |]
        readConfigFilesFrom global local `shouldReturn` Right mempty {
          beforeReload = Just "local-before-reload"
        , onSuccess = Just "local-on-success"
        }

    it "reads config values from ~/.config/sensei/sensei.yaml" $ do
      withTempDirectory $ \ dir -> do
        let
          global = dir </> "global.yaml"
          local = dir </> "local.yaml"
        LB.writeFile global $ encode [yamlQQ|
          on-failure: global-on-failure
          on-success: global-on-success
        |]
        readConfigFilesFrom global local `shouldReturn` Right mempty {
          onFailure = Just "global-on-failure"
        , onSuccess = Just "global-on-success"
        }

    context "with both ~/.config/sensei/sensei.yaml and ./sensei.yaml" $ do
      it "gives fields from ./sensei.yaml precedence" $ do
        withTempDirectory $ \ dir -> do
          let
            global = dir </> "global.yaml"
            local = dir </> "local.yaml"
          LB.writeFile global $ encode [yamlQQ|
            on-failure: global-on-failure
            on-success: global-on-success
          |]
          LB.writeFile local $ encode [yamlQQ|
            before-reload: local-before-reload
            on-success: local-on-success
          |]
          readConfigFilesFrom global local `shouldReturn` Right mempty {
            beforeReload = Just "local-before-reload"
          , onFailure = Just "global-on-failure"
          , onSuccess = Just "local-on-success"
          }

    context "with unknown fields" $ do
      it "returns an error" $ do
        withTempDirectory $ \ dir -> do
          let
            global = dir </> "global.yaml"
            local = dir </> "local.yaml"
          LB.writeFile local $ encode [yamlQQ|
            some-field: some-value
          |]
          readConfigFilesFrom global local `shouldReturn` Left "Aeson exception:\nError in $: parsing Config.ConfigFile(ConfigFile) failed, unknown fields: [\"some-field\"]"

  describe "ConfigFile" $ do
    it "deserializes from yaml" $ do
      fromJSON [yamlQQ|
        after-reload: foo bar baz
        before-reload: blah
        on-success: snafu
        on-failure: failure
      |] `shouldBe` Success ConfigFile {
        afterReload = Just "foo bar baz"
      , beforeReload = Just "blah"
      , onSuccess = Just "snafu"
      , onFailure = Just "failure"
      , deepSeek = Nothing
      }

    it "supports null" $ do
      fromJSON [yamlQQ|
        after-reload: null
        before-reload: blah
        on-success: finally
        on-failure: failure
      |] `shouldBe` Success ConfigFile {
        afterReload = Nothing
      , beforeReload = Just "blah"
      , onSuccess = Just "finally"
      , onFailure = Just "failure"
      , deepSeek = Nothing
      }

    it "supports missing fields" $ do
      fromJSON [yamlQQ|
        after-reload: blah
      |] `shouldBe` Success ConfigFile {
        afterReload = Just "blah"
      , beforeReload = Nothing
      , onSuccess = Nothing
      , onFailure = Nothing
      , deepSeek = Nothing
      }

    it "accepts deep-seek configuration values" $ do
      fromJSON [yamlQQ|
        deep-seek:
          auth: foo
      |] `shouldBe` Success @ConfigFile mempty {
        deepSeek = Just $ DeepSeek $ BearerToken "foo"
      }

    it "accepts an empty config" $ do
      fromJSON @ConfigFile [yamlQQ|
      |] `shouldBe` Success mempty
