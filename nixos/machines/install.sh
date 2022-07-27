set -e

RED='\e[0;31m'
GREEN='\e[0;32m'
NC='\e[0m'

step_count=0

error() {
    printf "[${GREEN}Step %02d${NC}] ${RED}FAILED${NC}: %s\n" "$step_count" "$*" >&2
    exit 1
}

step() {
    step_count=$((step_count + 1))
    printf "[${GREEN}Step %02d${NC}] %s\n" "$step_count" "$*"
}

sdone() {
    printf "[${GREEN}Step %02d${NC}] Complete.\n" "$step_count"
}

prompt() {
    read -rp "$1 [$(echo -e "${GREEN}Y${NC}/${RED}n${NC}")] " response
    case "$response" in
        [nN]*)
            false ;;
        *)
            true ;;
    esac
}

partition() {
    device="$(lsblk --bytes --noheadings --nodeps --output NAME,SIZE | sort --reverse \
                    --numeric-sort --key=2 | head -n1 | cut -d' ' -f1)"
    file="/dev/$device"

    if ! prompt "Will overwrite device $device. Is this reasonable?"; then
        lsblk --nodeps
        read -rp "Enter another device name: " device
    fi

    if [ ! -e "$file" ]; then
        error "$file does not exist."
    fi

    efi=0
    if [ -d "/sys/firmware/efi" ] && prompt "EFI appears to be supported. Use it?"; then
        efi=1
    fi

    root_start="0%"
    root_part="${file}1"
    if [ "$efi" -eq 0 ]; then
        parted -sa optimal "$file" mklabel msdos
    else
        parted -sa optimal "$file" mklabel gpt mkpart primary 0% 800M
        mkfs.vfat -n ESP "${file}1"
        root_start="800M"
        root_part="${file}2"
    fi

    parted -sa optimal "$file" mkpart primary "$root_start" 100%
    mkfs.ext4 -FL root "$root_part"

    while [ ! -e "/dev/disk/by-label/root" ]; do
        partprobe -s "$file"
        sleep 0.5
    done

    mount /dev/disk/by-label/root /mnt
    rm -rf /mnt/lost+found
    if [ "$efi" -ne 0 ]; then
        mkdir /mnt/boot
        mount /dev/disk/by-label/ESP /mnt/boot
    fi
}

if [ "$(id -u)" -ne 0 ]; then
    error "Installer must be run as root."
fi

step "Prepare storage."
echo "Automatic partitioning will choose the largest block device and wipe it completely."
if prompt "Use automatic partitioning?"; then
    partition
else
    echo "Dropping to a shell. Exit when done preparing storage. Mount root filesystem to /mnt and" \
         " ESP to /mnt/boot, if applicable."
    bash
fi
sdone


step "Fetch configuration."
git clone https://github.com/3541/dot.git
sdone

step "Bootstrap machine configuration."
nixos-generate-config --root /mnt
read -rp "Enter hostname: " hostname
config="dot/nixos/machines/${hostname}.nix"

cp /mnt/etc/nixos/hardware-configuration.nix "dot/nixos/machines/${hostname}-hardware.nix"
cp dot/nixos/machines/template.nix "$config"
sed -i "s/HOSTNAME/${hostname}/" "$config"
vim "$config"
sed -i "s/^\(.*\)\(# Add machines here\.\)/\1\"${hostname}\"\n\1\2/" dot/nixos/flake.nix
git -C dot add -A

rm /mnt/etc/nixos/configuration.nix /mnt/etc/nixos/hardware-configuration.nix
sdone

step "Install system."
nixos-install --flake "./dot/nixos#${hostname}" -L
sdone

step "Copy and link configuration."
cp -a dot /mnt/home/alex/dot
chown -R 1000:1000 /mnt/home/alex/dot
ln -s ../../home/alex/dot/nixos/flake.nix /mnt/etc/nixos/flake.nix
sdone

echo "Installation complete. Reboot when ready."
