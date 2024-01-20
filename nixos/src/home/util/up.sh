if command -v nixos-rebuild &> /dev/null; then
    if [ -e /etc/nixos/flake.nix ]; then
        cd ~/dot/nixos && nix flake update && cd -

        if command -v nom &> /dev/null; then
            sudo nixos-rebuild switch -L |& nom
        else
            sudo nixos-rebuild switch -L
        fi
    else
        sudo nixos-rebuild --upgrade-all switch

        if command -v home-manager &> /dev/null; then
            nix-channel --update
            home-manager switch
        fi
    fi
elif command -v dnf &> /dev/null; then
    sudo dnf upgrade
elif [ "$(uname)" = "Darwin" ]; then
    cd ~/dot/nixos && nix flake update && cd -
    nix run nix-darwin -- switch --flake "$HOME/dot/nixos" -L
    if [ -s ./result ]; then
        rm result
    fi
fi

if command -v flatpak &> /dev/null; then
    flatpak update
fi
