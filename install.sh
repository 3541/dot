#!/bin/sh

set -e

# macOS does not support readlink -f.
if [ "$(uname)" = "Darwin" ]; then
    cd $(perl -MCwd=abs_path -le 'print abs_path readlink(shift);' "$0")
else
    cd $(dirname $(readlink -f "$0"))
fi

if command -v nix &> /dev/null; then
    nix-shell -p stow --command 'stow nix'
    if [ ! -f nix/.config/nixpkgs/machines/current.nix ]; then
        ln -s "$(hostname).nix" nix/.config/nixpkgs/machines/current.nix
    fi

    if ! command -v home-manager &> /dev/null; then
        sudo nix-channel --add \
             https://github.com/nix-community/home-manager/archive/release-22.05.tar.gz \
             home-manager
        sudo nix-channel --update
        nix-shell '<home-manager>' -A install
    fi

    if [ -f /etc/NIXOS ]; then
        if [ ! -f nixos/machines/current.nix ]; then
            ln -s "$(hostname).nix" nixos/machines/current.nix
        fi
        sudo cp -r nixos/* /etc/nixos/
        sudo chown -R root:root /etc/nixos
        sudo chmod -R 600 /etc/nixos/configuration.nix /etc/nixos/machines /etc/nixos/src
        sudo chmod 700 /etc/nixos/machines /etc/nixos/src
    fi
else
    if [ ! -e "~/.bashrc.dist" && -e "~/.bashrc" ]; then
        mv "~/.bashrc" "~/.bashrc.dist"
    fi

    for d in bash i3 emacs git; do
        echo "$d"
        stow "$d"
    done

    if [ ! -e "~/.emacs.d/sensible-defaults.el" ]; then
        ln -s "$PWD/nix/.config/nixpkgs/src/programs/emacs-sensible-defaults.nix" \
           "~/.emacs.d/sensible-defaults.el"
    fi

    if [ ! -d "~/.emacs.d/emacs-color-theme-solarized" ]; then
        git clone https://github.com/sellout/emacs-color-theme-solarized.git \
            "~/.emacs.d/emacs-color-theme-solarized"
    fi
fi
