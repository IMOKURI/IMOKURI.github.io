#!/usr/bin/env stack
-- stack --resolver lts-8.21 --install-ghc runghc --package shake

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath

main :: IO ()
main = shakeArgs shakeOptions $ do
  phony "update" update

  phony "build" build

  phony "watch" $
    cmd "stack" "exec" "site" "watch"

  phony "reset" $
    cmd "git" "submodule" "foreach" "git" "reset" "--hard" "HEAD"

  phony "sdiff" $
    cmd (Cwd "_site") "git" "diff"

  phony "clean" $
    cmd "stack" "exec" "site" "clean"

  priority 0 $ action build


update :: Action ()
update = do
  () <- cmd "git" "pull"
  () <- cmd "git" "submodule" "init"
  () <- cmd "git" "submodule" "update"
  () <- cmd "git" "submodule" "foreach" "git" "pull" "origin" "master"
  cmd (Cwd "_site") "git" "checkout" "master"

build :: Action ()
build = do
  () <- cmd "stack" "build"
  withTempDir $ \dir -> do
    () <- cmd "mv" "_site/.git" dir
    () <- cmd "stack" "exec" "site" "rebuild"
    cmd "mv" (dir </> ".git") "_site/"
