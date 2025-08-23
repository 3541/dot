#!/bin/sh

set -e

install_nixos() {
    echo "TODO: nixos" >&2
    exit 1
}

install_nix_darwin() {
    if ! command -v brew > /dev/null 2>&1; then
        bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    fi

    sudo nix run nix-darwin/nix-darwin-25.05#darwin-rebuild -- switch --flake "$DOT_ROOT/flakes/darwin" -L
}

if [ "$(uname)" = "Darwin" ]; then
    install_nix_darwin
else
    install_nixos
fi
