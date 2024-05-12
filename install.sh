#!/bin/sh

set -e

# macOS does not support readlink -f.
if [ "$(uname)" = "Darwin" ]; then
    cd $(perl -MCwd=abs_path -le 'print abs_path readlink(shift);' "$0")
else
    cd $(dirname $(readlink -f "$0"))
fi

if command -v nix > /dev/null 2>&1; then
    if [ -f /etc/NIXOS ]; then
        if [ ! -f nixos/machines/$(hostname)-hardware.nix ] &&
               [ -f /etc/nixos/hardware-configuration.nix ]; then
            cp /etc/nixos/hardware-configuration.nix nixos/machines/$(hostname)-hardware.nix
        fi

        sudo ln -sf "$PWD/nixos/flake.nix" /etc/nixos/flake.nix
    elif [ "$(uname)" = "Darwin" ]; then
        echo "Building nix-darwin installer..."
        cd $(mktemp -d)
        nix-build https://github.com/LnL7/nix-darwin/archive/master.tar.gz -A installer

        echo "Installing nix-darwin..."
        ./result/bin/darwin-installer
        rm result
        cd -

        if [ ! -f "nixos/machines/$(hostname).nix" ]; then
            echo "Create machines/$(hostname).nix, then exit this shell."
            bash
        fi

        darwin-rebuild switch --flake ./nixos
    elif [ "$(uname)" = Linux ]; then
        echo "Building home-manager configuration..."
        nix build --no-link -L "./nixos/src/home#homeConfigurations.$(hostname).activationPackage"
        echo "Activating home-manager configuration..."
	nix run "./nixos/src/home#homeConfigurations.$(hostname).activationPackage"
    fi
else
    ./scripts/boot-$(uname).sh
    ./scripts/deps-$(uname).sh stow
    for dir in home/*; do
        pkg="$(echo $dir | cut -d'/' -f2)"
        stow --dotfiles --dir home --target "$HOME" "$pkg"
    done
fi
