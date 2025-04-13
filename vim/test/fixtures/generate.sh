#!/bin/bash
cabal exec -- runhaskell vim/test/fixtures/hspec.hs --expert --seed 0 --no-color > vim/test/fixtures/hspec.hs.errors
sed -i 's/Finished in 0.[0-9][0-9][0-9][0-9] seconds/Finished in 0.0005 seconds/' vim/test/fixtures/hspec.hs.errors

cd vim/test/fixtures

for name in $(ls *.hs | grep -v hspec); do
  for ghc in ghc-9.6 ghc-9.8 ghc-9.10; do
    if command -v "$ghc" >/dev/null 2>&1; then
      $ghc -fno-diagnostics-show-caret -fno-show-error-context "$name" &> "$name.ghc-$($ghc --numeric-version).errors"
    fi
  done
  for ghc in ghc-9.2 ghc-9.4; do
    if command -v "$ghc" >/dev/null 2>&1; then
      $ghc -fno-diagnostics-show-caret "$name" &> "$name.ghc-$($ghc --numeric-version).errors"
    fi
  done
done
