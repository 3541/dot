if command -v nixos-rebuild &> /dev/null; then
    sudo nixos-rebuild --upgrade-all switch
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

nix-channel --update
home-manager switch

if command -v flatpak; then
    flatpak update
fi
