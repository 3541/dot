if [ "$(uname)" = "Darwin" ]; then
    cd ~/dot/flakes/darwin
    nix flake update
    morlana switch --flake .
elif [ ! -f /etc/NIXOS ]; then
    cd ~/dot/flakes/linux-home-manager
    nix flake update

    package=".#homeConfigurations.$(hostname).activationPackage"
    nom build "$package"
    nix run "$package"
else
    echo "$(uname) not supported." >&2
    exit 1
fi
