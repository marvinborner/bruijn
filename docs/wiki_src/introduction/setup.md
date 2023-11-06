# Setup

In theory you can use any common editor and operating system. Please
create an [issue on
GitHub](https://github.com/marvinborner/bruijn/issues/new) if you have
problems setting up bruijn.

## Recommended setup

The recommended setup is to use [Vim](https://www.vim.org/) and its
bruijn plugin.

The Vim plugin adds syntax highlighting for bruijn files as well as a
custom keymap for typing commonly used unicode symbols.

### Installation

1.  Use Vim and [vim-plug](https://github.com/junegunn/vim-plug) (or
    comparable plugin managers)
2.  Add `"Plug 'marvinborner/bruijn', { 'rtp': 'editors/vim' }"` to your
    `.vimrc`
3.  Run `:PlugInstall`

### Unicode abbreviations

You can find all abbreviations in
[`editors/vim/syntax/bruijn.vim`](https://github.com/marvinborner/bruijn/blob/main/editors/vim/syntax/bruijn.vim).
Abbreviations get replaced after pressing space or `C-]`. Feel free to
suggest improvements or create your own abbreviations.

## Alternatives

We have a Kate XML syntax highlighting file in
[`editors/kate/bruijn.xml`](https://github.com/marvinborner/bruijn/blob/main/editors/kate/bruijn.xml),
although it has *not* actually been tested with Kate (only as a syntax
highlighting file for `pandoc`).

## Broogle

Broogle is a tool for searching standard library functions by name, type
signatures, or comment. It's highly inspired by Haskell's
[hoogle](https://hoogle.haskell.org/).

You can use it after cloning [bruijn's
repository](https://github.com/marvinborner/bruijn) and installing `rg`
(`ripgrep`), `jq`, `sed`, and `awk`.

``` bash
./broogle.sh -t "a -> a"
./broogle.sh -f "i"
./broogle.sh -c "idiot combinator"
```
