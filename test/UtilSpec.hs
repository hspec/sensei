module UtilSpec (spec) where

import           Helper

import qualified System.Console.ANSI as Ansi
import           System.Posix.Files

import           Util

withDotGhci :: (FilePath -> IO a) -> IO a
withDotGhci action = withTempDirectory $ \ dir -> do
  let dotGhci = dir </> ".ghci"
  writeFile dotGhci ""
  chmod "go-w" dotGhci
  action dotGhci

chmod :: String -> FilePath -> IO ()
chmod mode file = callProcess "chmod" [mode, file]

spec :: Spec
spec = do
  describe "stripAnsi" $ do
    it "removes ANSI color sequences" $ do
      stripAnsi ("some " <> withColor Green "colorized" <> " text") `shouldBe` "some colorized text"

    it "removes DEC private mode sequences" $ do
      stripAnsi (Ansi.hideCursorCode <> "some text" <> Ansi.showCursorCode) `shouldBe` "some text"

  describe "isBoring" $ do
    it "ignores files in .git/" $ do
      isBoring "/foo/bar/.git/baz/foo.txt" `shouldBe` True

    it "ignores files in dist/" $ do
      isBoring "/foo/bar/dist/baz/foo.txt" `shouldBe` True

  describe "normalizeTypeSignatures" $ do
    it "removes newlines from type signatures" $ do
      let signature = "foo\n  :: IO\n       Int\n"
      normalizeTypeSignatures signature `shouldBe` "foo :: IO Int\n"

    it "replaces unicode characters" $ do
      normalizeTypeSignatures "head ∷ [a] → a" `shouldBe` "head :: [a] -> a"

  let gitlessFeedback = Just (Cyan, "warning: not a git repository - .gitignore support not available\n")

  describe "filterGitIgnoredFiles_" $ do
    it "discards files that are ignored by git" $ do
      withTempDirectory $ \ dir -> do
        _ <- readProcess "git" ["-C", dir, "init", "--initial-branch=main"] ""
        writeFile (dir </> ".gitignore") "foo"
        filterGitIgnoredFiles_ dir ["foo", "bar"] `shouldReturn` (Nothing, ["bar"])

    context "when used outside of a git repository" $ do
      it "returns all files" $ do
        withTempDirectory $ \ dir -> do
          filterGitIgnoredFiles_ dir ["foo", "bar"] `shouldReturn` (gitlessFeedback, ["foo", "bar"])

  describe "gitCheckIgnoreFeedback" $ do
    context "when git reports no repository" $ do
      it "returns gitless warning" $ do
        let err = "fatal: not a git repository (or any of the parent directories): .git\n"
        gitCheckIgnoreFeedback err `shouldBe` gitlessFeedback

    context "when git stops at a filesystem boundary" $ do
      it "returns gitless warning" $ do
        let err = "fatal: not a git repository (or any parent up to mount point /)\nStopping at filesystem boundary (GIT_DISCOVERY_ACROSS_FILESYSTEM not set).\n"
        gitCheckIgnoreFeedback err `shouldBe` gitlessFeedback

    context "when git reports no error" $ do
      it "returns Nothing" $ do
        gitCheckIgnoreFeedback "" `shouldBe` Nothing

    context "when git reports any other error" $ do
      it "returns the error" $ do
        let err = "fatal: Unable to do such and such"
        gitCheckIgnoreFeedback err `shouldBe` Just (Red, err)

  describe "isWritableByOthers" $ do
    it "returns False" $ do
      withDotGhci $ \ dotGhci -> do
        isWritableByOthers dotGhci `shouldReturn` False

    context "when writable by group" $ do
      it "returns True" $ do
        withDotGhci $ \ dotGhci -> do
          chmod "g+w" dotGhci
          isWritableByOthers dotGhci `shouldReturn` True

    context "when writable by others" $ do
      it "returns True" $ do
        withDotGhci $ \ dotGhci -> do
          chmod "o+w" dotGhci
          isWritableByOthers dotGhci `shouldReturn` True

    context "when directory is writable by group" $ do
      it "returns True" $ do
        withDotGhci $ \ dotGhci -> do
          chmod "g+w" (takeDirectory dotGhci)
          isWritableByOthers dotGhci `shouldReturn` True

    context "when .ghci does not exist" $ do
      it "returns False" $ do
        withTempDirectory $ \ dir -> do
          let dotGhci = dir </> ".ghci"
          isWritableByOthers dotGhci `shouldReturn` False

  describe "writableByOthers" $ do
    it "returns False for owner" $ do
      writableByOthers ownerWriteMode `shouldBe` False

    it "returns True for group" $ do
      writableByOthers groupWriteMode `shouldBe` True

    it "returns True for others" $ do
      writableByOthers otherWriteMode `shouldBe` True
