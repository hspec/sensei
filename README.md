__THIS IS EXPERIMENTAL STUFF! USE AT YOUR OWN RISK!__

# 先生 (*rōmaji* sensei)

In it's simplest form, you run `sensei` with the `Main` module of your test
suite as an argument:

    sensei test/Spec.hs

Note that `sensei` picks up options from `.ghci`-files.  You can provide
additional GHC options on the command line:

    sensei -isrc -itest test/Spec.hs

Command-line arguments that look like Hspec options are passed to Hspec.  To
avoid ambiguity, GHC options have to be given before any Hspec options:

    sensei -isrc -itest test/Spec.hs --no-color --match foo

All command-line arguments after the last `--` are passed to Hspec, regardless
how they look:

    sensei -isrc -itest test/Spec.hs -- --no-color --match foo

## Using `sensei` with Cabal sandboxes

    cabal exec sensei test/Spec.hs

## Accessing result on the command-line

You can access the results of the last test run with `seito`:

    seito

Alternatively, if you have `curl` version `7.40.0` or newer, you can use `curl`
instead:

    curl --unix-socket .sensei.sock http://localhost/


### Vim integration

Create a Makefile with the following content:

```Makefile
all:
	@seito | sed 's/\x1B\[[0-9;]*[JKmsu]//g'
```

(`sed` is used to strip ANSI color sequences)

You can then load the result of the last test run into your quickfix list by
executing `:make` in Vim.
