#!/bin/sh

set -e

cd $(dirname $(readlink -f "$0"))

if command -v nix; then
    nix-shell -p stow --command 'stow nix'
else
    for d in bash emacs i3 nvim sakura redshift git; do
        echo "$d"
        stow "$d"
    done
fi
