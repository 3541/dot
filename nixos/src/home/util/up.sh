if command -v nixos-rebuild &> /dev/null; then
    if [ -e /etc/nixos/flake.nix ]; then
        cd ~/dot/nixos && nix flake update && cd -
        sudo nixos-rebuild switch -L
    else
        sudo nixos-rebuild --upgrade-all switch
    fi
elif command -v dnf &> /dev/null; then
    sudo dnf upgrade
elif [ "$(uname)" = "Darwin" ]; then
    sudo -i sh -c \
         'nix-channel --update && nix-env -iA nixpkgs.nix && launchctl remove org.nixos.nix-daemon && launchctl load /Library/LaunchDaemons/org.nixos.nix-daemon.plist'
    MACPORTS=/opt/local/bin/port
    if [ -f "$MACPORTS" ]; then
        sudo "$MACPORTS" selfupdate && sudo "$MACPORTS" upgrade outdated
    fi
fi

if [ ! -e /etc/nixos/flake.nix ]; then
    nix-channel --update
    home-manager switch
fi

if command -v flatpak &> /dev/null; then
    flatpak update
fi
