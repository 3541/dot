set -euo pipefail

GREEN='\e[0;32m'
BLUE='\e[0;34m'
NC='\e[0m'
INFO_PREFIX="[${BLUE}%-17s${NC}]"

info() {
    printf "${INFO_PREFIX} %b\n" "$1" "$2"
}

idone() {
    info "$1" "$2 ${GREEN}Done.${NC}"
}

v() {
    virsh -c qemu:///system "$@"
}

vm_run() (
    vm="$1"
    command="$2"
    status_file="$3"

    if [[ -f "$status_file" ]] && grep -q "$command" "$status_file" && \
           grep -q "$vm" "$status_file"; then
        info "$vm" "Already run."
        return
    fi

    if [[ "$vm" = *"win"* ]] || [[ "$vm" = *"guix"* ]] || [[ "$vm" = *"ubuntu16.04"* ]]; then
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
    rsync -a -e ssh --exclude='/.git' --filter=':- .gitignore' . "$ip":"$tmpdir/"
    if [[ -d "subprojects" ]]; then
        rsync -a -e ssh subprojects/* "$ip":"$tmpdir/subprojects/"
    fi
    idone "$vm" "Copying directory... "

    info "$vm" "Running command..."
    ssh alex@"$ip" "cd $tmpdir && $command" 2>&1 | sed "s/^/$(printf $INFO_PREFIX $vm) /"
    idone "$vm" "Running command..."

    info "$vm" "Cleaning up..."
    ssh alex@"$ip" "rm -rf $tmpdir"
    idone "$vm" "Cleaning up..."

    timeout 3 ssh alex@"$ip" "\$SHELL -l -c 'sudo poweroff'" 2>&1 \
        | sed "s/^/$(printf $INFO_PREFIX $vm) /" || true

    echo "$vm" >> "$status_file"
)

mkdir -p ~/.cache/vrun
if [[ ! -f ~/.cache/vrun/progress ]] || ! grep -q "$*" ~/.cache/vrun/progress; then
    echo "$*" > ~/.cache/vrun/progress
fi

for vm in $(v list --all | tail -n '+3' | awk '{ print $2 }'); do
    vm_run "$vm" "$*" ~/.cache/vrun/progress
done

rm -f ~/.cache/vrun/progress

wait
