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
    user="alex"

    mkdir -p ~/.cache/vup
    timestamp_file="$HOME/.cache/vup/$vm"
    if [[ -f "$timestamp_file" ]] && [[ "$(($(cat $timestamp_file) + 60 * 60 * 24))" -ge "$(date '+%s')" ]]; then
        info "$vm" "Already updated."
        return
    fi

    if [[ "$vm" = *"win"* ]] || [[ "$vm" = *"guix"* ]]; then
        info "$vm" "SKIPPED."
        return
    fi

    if [[ "$vm" = *"haiku"* ]]; then
        user="user"
    fi

    info "$vm" "Starting..."
    v start "$vm" > /dev/null

    case "$vm" in
        *gentoo*)
            ip="192.168.0.6"
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

    while ! ssh "${user}@${ip}" exit 0 2> /dev/null; do
        sleep 5
    done
    idone "$vm" "Starting..."
    info "$vm" "$ip"

    info "$vm" "Updating... "
    ssh "${user}@${ip}" "\$SHELL -l -c 'up'" 2>&1 | sed "s/^/$(printf $INFO_PREFIX $vm) /"
    if [[ "$vm" = *"haiku" ]]; then
        ssh "${user}@${ip}" "shutdown" 2>&1 | sed "s/^/$(printf $INFO_PREFIX $vm) /"
    else
        timeout 3 ssh "${user}@${ip}" "\$SHELL -l -c 'sudo poweroff'" 2>&1 \
            | sed "s/^/$(printf $INFO_PREFIX $vm) /" || true
        fi
    idone "$vm" "Updating..."

    date '+%s' > "$timestamp_file"
)

for vm in $(v list --inactive | tail -n '+3' | awk '{ print $2 }'); do
    update_vm "$vm"
done

wait
