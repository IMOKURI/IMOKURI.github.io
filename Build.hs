#!/usr/bin/env stack
-- stack --resolver lts-6.16 --install-ghc runghc --package shake

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath

main :: IO ()
main = shakeArgs options $ do
  phony "build" $ do
    () <- cmd "stack" "build"
    withTempDir $ \dir -> do
      () <- cmd "mv" "_site/.git" dir
      () <- cmd "stack" "exec" "site" "rebuild"
      cmd "mv" (dir </> ".git") "_site/"

  phony "watch" $
    cmd "stack" "exec" "site" "watch"

  phony "reset" $ do
    () <- cmd "stack" "exec" "site" "clean"
    () <- cmd "git" "submodule" "init"
    () <- cmd "git" "submodule" "update"
    cmd "git" "submodule" "foreach" "git" "pull" "origin" "master"

options :: ShakeOptions
options = shakeOptions
  { shakeProgress = progressSimple
  }
