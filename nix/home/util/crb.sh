set -eu

update=
target=
while getopts "ut:" o; do
    case "$o" in
        u)
            update=1
            ;;
        t)
            target="$OPTARG"
            ;;
        *)
            echo "unrecognized: $o" >&1
            exit 1
            ;;
    esac
done

if [ -z "$target" ]; then
    case "$(uname)" in
        Darwin)
            target_os=darwin
            ;;
        Linux)
            if [ -f /etc/NIXOS ]; then
                target_os=nixos
            else
                target_os=linux
            fi
            ;;
        *)
            echo "unrecognized system: $(uname)" >&1
            exit 1
            ;;
    esac
else
    # TODO: Others.
    target_os=nixos
fi

case "$target_os" in
    darwin)
        cd ~/dot/flakes/darwin

        if [ -n "$update" ]; then
            nix flake update
        fi

        nh darwin switch .
        ;;
    nixos)
        cd ~/dot/flakes/nixos

        if [ -n "$update" ]; then
            nix flake update
        fi

        if [ -z "$target" ]; then
            nh os switch .
        else
            nh os switch --target-host "$target" --hostname "$target" .
        fi
        ;;
    linux)
        cd ~/dot/flakes/linux-home-manager

        if [ -n "$update" ]; then
            nix flake update
        fi

        nh home switch --configuration "$(hostname)" .
        ;;
    *)
        echo "???" >&1
        exit 1
        ;;
esac
