#!/bin/sh

set -e

if command -v nix > /dev/null 2>&1; then
    exit 0
fi

curl --proto '=https' --tlsv1.2 -sSf -L https://install.lix.systems/lix | sh -s -- install \
  --no-confirm

NU_VENDOR_PATH=/usr/local/share/nushell/vendor/autoload

sudo mkdir -p "$NU_VENDOR_PATH"
sudo cp "$SCRIPTS/nix-daemon.nu" "$NU_VENDOR_PATH/nix-daemon.nu"

. /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh

if ! command -v nix > /dev/null 2>&1; then
    echo "nix not found after installation" >&2
    exit 1
fi
