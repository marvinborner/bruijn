# Vim syntax highlighting

## Install manually

Copy or symlink this directory to your vim plugins directory, e.g. using
the following command inside this directory:

    mkdir -p $HOME/.vim/pack/plugins/start/
    ln -s $PWD $HOME/.vim/pack/plugins/start/bruijn

## Install with a plugin manager

In this example using vim-plug (others should work similarly):

1.  Add `Plug 'marvinborner/bruijn', { 'rtp': 'editors/vim' }` to your
    `.vimrc`
2.  Run `:PlugInstall`

# Snippets

Snippets requires UltiSnips or a similar (compatible) alternative. You
can then install the snippets using something like this (depends on your
path setup):

    ln -s $PWD/bruijn.snippets $HOME/.vim/plugged/vim-snippets/UltiSnips/bruijn.snippets
