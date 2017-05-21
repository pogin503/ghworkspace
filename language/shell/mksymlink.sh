#!/bin/sh

makeSymlink() {
    if [ ! -e "$2" ];then
        ln -s "$1" "$2"

        [ $? -eq 0 ] && echo "Create $1 to $2 symbolic link."
    else
        echo "nil"
    fi
}

DROPBOX_DIR="$HOME/Dropbox"

makeSymlink "$DROPBOX_DIR"/vim/.vimrc  ~/.vimrc
makeSymlink "$DROPBOX_DIR"/zsh/.zshenv ~/.zshenv
makeSymlink "$DROPBOX_DIR"/ssh         ~/.ssh
makeSymlink "$DROPBOX_DIR"/.gitconfig  ~/.gitconfig
makeSymlink "$DROPBOX_DIR"/.gem        ~/.gem
makeSymlink "$DROPBOX_DIR"/.rbenv      ~/.rbenv
makeSymlink "$DROPBOX_DIR"/.vagrant.d/ ~/.vagrant.d
makeSymlink "$DROPBOX_DIR"/.composer/  ~/.composer
makeSymlink "$DROPBOX_DIR"/.WebIde80/  ~/.WebIde80
