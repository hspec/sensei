__THIS IS EXPERIMENTAL STUFF! USE AT YOUR OWN RISK!__

# 先生 (*rōmaji*: sensei)

In its simplest form, you run `sensei` with the `Main` module of your test
suite as an argument:

    sensei test/Spec.hs

Note that `sensei` picks up options from `.ghci`-files.  You can provide
additional GHC options on the command line:

    sensei -isrc -itest test/Spec.hs

Command-line arguments that look like Hspec options are passed to Hspec:

    sensei -isrc -itest test/Spec.hs --no-color --match foo

Hspec's `-f` option collides with GHC flags.  To avoid ambiguity, `sensei` does
not accept Hspec's `-f` option.  Use `--format` instead:

    sensei -isrc -itest test/Spec.hs --format progress -fdiagnostics-as-json

A `--` disables any command-line processing.  All command-line arguments after
the last `--` are unconditionally passed to Hspec:

    sensei -isrc -itest test/Spec.hs -- --no-color --match foo

### Warnings and errors

By default, `sensei` treats warnings as errors.  You can pass `-Wwarn` on the
command line to prevent this behavior.  However, consider to adjust the warning
behavior instead (e.g. through `-w`, `-Wdefault`, `-Wall`).

### Boring files

When `sensei` is used inside a Git repository, it ignores modifications to
[files that are ignored by `git`](https://git-scm.com/docs/gitignore).

### Using `sensei` with Cabal sandboxes

    cabal exec sensei test/Spec.hs

## 生徒 (*rōmaji*: seito): Accessing results on the command line

You can access the results of the last test run with `seito`:

    seito

Alternatively, if you have `curl` version `7.40.0` or newer, you can use `curl`
instead:

    curl --unix-socket .sensei.sock http://localhost/


### Vim integration

You can use `seito` to load the results of the last test run into your quickfix
list by executing `:make` in Vim.

For this to work, you can choose one out of three options:

1. Create a `Makefile`
2. Set `makeprg` to a custom value
3. Use [`sensei.vim`](vim/sensei.vim)

#### Option 1: Create a `Makefile`

Create a Makefile with the following content:

```Makefile
all:
	@seito
```


#### Option 2: Set `makeprg`:

Add the following to your Vim configuration (e.g.
`~/.vim/after/ftplugin/haskell.vim`):

```vim
:set makeprg=seito
```

#### Option 3: Use `sensei.vim`:

Add the following to your Vim configuration (e.g.
`~/.vim/after/ftplugin/haskell.vim`):

```vim
execute 'source ' . system('seito --vim-config')
```

### Emacs integration

Similarly, you can use `sensei` to load the result of the last test run into an
Emacs buffer by executing `M-x compile` in Emacs.

For this to work, you can create a `Makefile` as described in Option 1 above.
