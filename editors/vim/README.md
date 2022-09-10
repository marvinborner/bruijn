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

# Autocompletion

This plugin uses vim’s `abbreviation`s for autocompletion. You can
complete them using `space` or `C-]`
