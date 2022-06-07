set -euo pipefail

GREEN='\e[0;32m'
BLUE='\e[0;34m'
NC='\e[0m'
INFO_PREFIX="[${BLUE}%-21s${NC}]"

info() {
    printf "${INFO_PREFIX} %b\n" "$1" "$2"
}

idone() {
    info "$1" "$2 ${GREEN}Done.${NC}"
}

v() {
    virsh -c qemu:///system "$@"
}

update_vm() (
    vm="$1"
    command="$2"

    if [[ "$vm" = *"win"* ]] || [[ "$vm" = *"guix"* ]]; then
        info "$vm" "SKIPPED."
        return
    fi

    info "$vm" "Starting..."
    v start "$vm" &> /dev/null || true

    case "$vm" in
        *gentoo*)
            ip="192.168.1.79"
            ;;
        *macOS*)
            while ! grep -q "macos" <(v net-dhcp-leases default); do
                sleep 5
            done

            ip=$(v net-dhcp-leases default | grep "macos" | awk '{ print $5 }' | cut -d '/' -f1)
            ;;
        *)
            while ! grep -q "ipv4" <(v domifaddr "$vm"); do
                sleep 5
            done

            ip=$(v domifaddr "$vm" | tail -n '+3' | awk '{ print $4 }' | cut -d'/' -f1)
            ;;
    esac

    while ! ssh alex@"$ip" exit 0 2> /dev/null; do
        sleep 5
    done
    idone "$vm" "Starting..."
    info "$vm" "$ip"

    info "$vm" "Copying directory... "
    tmpdir=$(ssh alex@"$ip" "mktemp -d")
    scp -rq . alex@"$ip":"$tmpdir/"
    idone "$vm" "Copying directory... "

    info "$vm" "Running command..."
    ssh alex@"$ip" "cd $tmpdir && $command" 2>&1 | sed "s/^/$(printf $INFO_PREFIX $vm) /"
    idone "$vm" "Running command..."

    info "$vm" "Cleaning up..."
    ssh alex@"$ip" "rm -rf $tmpdir"
    idone "$vm" "Cleaning up..."

    timeout 3 ssh alex@"$ip" "\$SHELL -l -c 'sudo poweroff'" 2>&1 \
        | sed "s/^/$(printf $INFO_PREFIX $vm) /" || true
)

for vm in $(v list | tail -n '+3' | awk '{ print $2 }'); do
    update_vm "$vm" "$*"
done

wait
