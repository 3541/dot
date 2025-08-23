if [ "$(uname)" = "Darwin" ]; then
    cd ~/dot/flakes/darwin
    nix flake update
    morlana switch --flake .
else
    echo "$(uname) not supported." >&2
    exit 1
fi
