set -eu

update=
while getopts "u" o; do
    case "$o" in
        u)
            update=1
            ;;
        *)
            echo "unrecognized: $o" >&1
            exit 1
            ;;
    esac
done

if [ "$(uname)" = "Darwin" ]; then
    cd ~/dot/flakes/darwin

    if [ -n "$update" ]; then
        nix flake update
    fi
    morlana switch --flake .
elif [ ! -f /etc/NIXOS ]; then
    cd ~/dot/flakes/linux-home-manager

    if [ -n "$update" ]; then
        nix flake update
    fi

    package=".#homeConfigurations.$(hostname).activationPackage"
    nom build "$package" --no-link
    nix run "$package"
else
    echo "$(uname) not supported." >&2
    exit 1
fi
