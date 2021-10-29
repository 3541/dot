#!/bin/sh

set -e

cd $(dirname $(readlink -f "$0"))

if command -v nix; then
    nix-shell -p stow --command 'stow nix'
    sudo nix-channel --add https://github.com/nix-community/home-manager/archive/release-21.05.tar.gz home-manager
    sudo nix-channel --update
    nix-shell '<home-manager>' -A install
else
    for d in bash emacs i3 nvim sakura redshift git; do
        echo "$d"
        stow "$d"
    done
fi
