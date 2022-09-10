if command -v nixos-rebuild &> /dev/null; then
    if [ -e /etc/nixos/flake.nix ]; then
        cd ~/dot/nixos && nix flake update && cd -
        sudo nixos-rebuild switch -L
    else
        sudo nixos-rebuild --upgrade-all switch

        if command -v home-manager &> /dev/null; then
            nix-channel --update
            home-manager switch
        fi
    fi
elif command -v dnf &> /dev/null; then
    sudo dnf upgrade
elif command -v darwin-rebuild &> /dev/null; then
    cd ~/dot/nixos && nix flake update && cd -
    darwin-rebuild switch --flake "$HOME/dot/nixos" -L
    if [ -s ./result ]; then
        rm result
    fi
fi

if command -v flatpak &> /dev/null; then
    flatpak update
fi
