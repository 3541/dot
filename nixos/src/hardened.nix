{ lib, config, pkgs, ... }:
let cfg = config.a3;
in {
  # TODO: Work out why this breaks network filesystems.
  # imports = [ <nixpkgs/nixos/modules/profiles/hardened.nix> ];

  config = {
    # Revert some bad settings.
    security.allowUserNamespaces = true;
    security.allowSimultaneousMultithreading = true;
    # Hardened kernel patches are currently not working.
    boot.kernelPackages = pkgs.linuxPackages;
    # SCUDO crashes Firefox.
    environment.memoryAllocator.provider = "libc";
  };
}
