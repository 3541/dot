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

run_command() (
    ip="$1"
    command="$2"

    if ssh "$ip" uname &> /dev/null; then
        ssh "$ip" "$command"
    else
        ssh "$ip" "bash -c \"$command\""
    fi
)

vm_run() (
    vm="$1"
    command="$2"
    status_file="$3"
    user="alex"

    if [[ -f "$status_file" ]] && grep -q "$command" "$status_file" && \
           grep -q "$vm" "$status_file"; then
        info "$vm" "Already run."
        return
    fi

    if [[ "$vm" = *"guix"* ]]; then
        info "$vm" "SKIPPED."
        return
    fi

    if [[ "$vm" = *"haiku"* ]]; then
        user="user"
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

    while ! ssh "${user}@${ip}" exit 0 2> /dev/null; do
        sleep 5
    done
    idone "$vm" "Starting..."
    info "$vm" "$ip"

    info "$vm" "Copying directory... "
    if [[ "$vm" != *"win"* ]]; then
        tmpdir="$(ssh "${user}@${ip}" "mktemp -d")"
        tmpdir_rsync="$tmpdir"
    else
        tmpdir="AppData/Local/Temp/$(mktemp -u XXXXXX)"
        ssh "$ip" "bash -c \"mkdir -p $tmpdir\""
    fi
    rsync -a -e ssh --exclude='/.git' --filter=':- .gitignore' . "${user}@${ip}":"$tmpdir/"
    if [[ -d "subprojects" ]]; then
        rsync -a -e ssh subprojects/* "${user}@${ip}":"$tmpdir/subprojects/"
    fi
    idone "$vm" "Copying directory... "

    info "$vm" "Running command..."
    run_command "${user}@${ip}" "cd $tmpdir && \"$command\"" 2>&1 | \
        sed "s/^/$(printf $INFO_PREFIX $vm) /"
    idone "$vm" "Running command..."

    info "$vm" "Cleaning up..."
    run_command "${user}@${ip}" "rm -rf $tmpdir"
    idone "$vm" "Cleaning up..."

    if [[ "$vm" = *"haiku"* ]]; then
        ssh "${user}@${ip}" "shutdown" 2>&1 | sed "s/^/$(printf $INFO_PREFIX $vm) /"
    elif [[ "$vm" != *"win"* ]]; then
        timeout 3 ssh "$ip" "\$SHELL -l -c 'sudo poweroff'" 2>&1 \
            | sed "s/^/$(printf $INFO_PREFIX $vm) /" || true
    else
        ssh "$ip" "shutdown /s" 2>&1 | sed "s/^/$(printf $INFO_PREFIX $vm) /" || true
    fi

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
