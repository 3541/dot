#!/bin/sh

set -e

# macOS does not support readlink -f.
if [ "$(uname)" = "Darwin" ]; then
    cd $(perl -MCwd=abs_path -le 'print abs_path readlink(shift);' "$0")
else
    cd $(dirname $(readlink -f "$0"))
fi

if command -v nix &> /dev/null; then
    if [ -f /etc/NIXOS ]; then
        if [ ! -f nixos/machines/$(hostname)-hardware.nix ] &&
               [ -f /etc/nixos/hardware-configuration.nix ]; then
            cp /etc/nixos/hardware-configuration.nix nixos/machines/$(hostname)-hardware.nix
        fi

        sudo ln -sf /etc/nixos/flake.nix "$PWD/nixos/flake.nix"
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
