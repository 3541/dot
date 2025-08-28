#!/bin/sh

set -e

RELEASE=25.05

install_nix_darwin() {
    if ! command -v brew > /dev/null 2>&1; then
        bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    fi

    sudo nix run "nix-darwin/nix-darwin-${RELEASE}#darwin-rebuild" -- switch --flake "$DOT_ROOT/flakes/darwin" -L
}

install_nixos() {
    echo "TODO: nixos" >&2
    exit 1
}

install_home_manager() {
    package="./flakes/linux-home-manager#homeConfigurations.$(hostname).activationPackage"
    nix build --no-link -L "$package"
    nix run "$package"
}

if [ "$(uname)" = "Darwin" ]; then
    install_nix_darwin
elif [ -f /etc/NIXOS ]; then
    install_nixos
else
    install_home_manager
fi
