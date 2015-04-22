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

## Using `autospec` with Cabal sandboxes

    cabal exec autospec test/Spec.hs

## Accessing result with `curl`

__Note:__ This only works with `curl` version `7.40.0` and newer.

    curl --unix-socket .autospec.sock http://localhost

### Vim integration

Create a Makefile with the following content:

```Makefile
all:
        curl --silent --unix-socket .autospec.sock http://localhost | sed 's/\x1B\[[0-9;]*[JKmsu]//g'
```

(`sed` is used to strip ANSI color sequences)

You can then load the result of the last test run into your quick fix list by
executing `:make` in Vim.
