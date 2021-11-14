#!/bin/sh

set -e

cd $(dirname $(readlink -f "$0"))

if command -v nix &> /dev/null; then
    nix-shell -p stow --command 'stow nix'
    if [ ! -f nix/.config/nixpkgs/machines/current.nix ]; then
        ln -s $(hostname).nix nix/.config/nixpkgs/machines/current.nix
    fi

    if ! command -v home-manager &> /dev/null; then
        sudo nix-channel --add https://github.com/nix-community/home-manager/archive/release-21.05.tar.gz home-manager
        sudo nix-channel --update
        nix-shell '<home-manager>' -A install
    fi

    if [ -f /etc/NIXOS ]; then
        if [ ! -f nixos/machines/current.nix ]; then
            ln -s $(hostname).nix nixos/machines/current.nix
        fi
        sudo cp -r nixos/* /etc/nixos/
        sudo chown -R root:root /etc/nixos
        sudo chmod -R 600 /etc/nixos/configuration.nix /etc/nixos/machines /etc/nixos/src
        sudo chmod 700 /etc/nixos/machines /etc/nixos/src
    fi
else
    for d in bash emacs i3 nvim sakura redshift git; do
        echo "$d"
        stow "$d"
    done
fi
