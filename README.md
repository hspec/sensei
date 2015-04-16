# autospec

In it's simplest form, you run `autospec` with the `Main` module of your test
suite as an argument:

    autospec test/Spec.hs

Note that `autospec` picks up options from `.ghci`-files.  You can provide
additional GHC options on the command line:

    autospec -isrc -itest test/Spec.hs

Command-line arguments that look like Hspec options are passed to Hspec.  To
avoid ambiguity, GHC options have to be given before any Hspec options:

    autospec -isrc -itest test/Spec.hs --no-color --match foo

All command-line arguments after the last `--` are passed to Hspec, regardless
how they look:

    autospec -isrc -itest test/Spec.hs -- --no-color --match foo
