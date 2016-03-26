__THIS IS EXPERIMENTAL STUFF! USE AT YOUR OWN RISK!__

# 先生 (*rōmaji*: sensei)

(`sensei` is Japanese for 'teacher'.)

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

## 生徒 (*rōmaji*: seito): Accessing result on the command-line

(`seito` is Japanese for 'student'.)

You can access the results of the last test run with `seito`:

    seito

Alternatively, if you have `curl` version `7.40.0` or newer, you can use `curl`
instead:

    curl --unix-socket .sensei.sock http://localhost/


### Vim integration

You can use `sensei` to load the result of the last test run into your quickfix
list by executing `:make` in Vim.

For this to work you can either create a `Makefile` or set `makeprg` to a
custom value.

(In both cases `sed` is used to strip ANSI color sequences.)

#### Option 1: Create a `Makefile`

Create a Makefile with the following content:

```Makefile
all:
	@seito | sed 's/\x1B\[[0-9;]*[JKmsu]//g'
```

#### Option 2: Set `makeprg`:

Add the following to your Vim configuration (e.g.
`~/.vim/after/ftplugin/haskell.vim`):

```vim
:set makeprg=seito\ \\\|\ sed\ 's/\\x1B\\[[0-9;]*[JKmsu]//g'
```
You can then load the result of the last test run into your quickfix list by
executing `:make` in Vim.

## Q & A

Q: My tests are pretty slow, how can I use `sensei` to only typecheck my code but not run any tests?
A: Pass `-fno-code` to `sensei` like: `sensei -fno-code test/Spec.hs`
