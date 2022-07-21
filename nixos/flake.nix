{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-22.05";
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    # Old nixpkgs with kernel 5.17.14 for bcachefs. See machines/spirit.nix.
    nixpkgs-bcachefs.url = "nixpkgs/efb6eb853e8b1546ad7760a31159bf1a8ea132b3";
    nixpkgs-unstable.url = "nixpkgs/nixos-unstable";

    a3 = {
      url = "./src";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, ... }@args: {
    nixosConfigurations = builtins.listToAttrs (map (name: {
      name = name;
      value = nixpkgs.lib.nixosSystem
        ((import ./machines/${name}.nix args) // { specialArgs = args; });
    }) [ "opportunity" "spirit" "charon" "sagittarius" "netboot-installer" ]);

    packages.x86_64-linux.charonImage =
      self.nixosConfigurations.charon.config.system.build.sdImage;
    packages.x86_64-linux.pxeInstallImage = nixpkgs.legacyPackages.x86_64-linux.pkgs.symlinkJoin {
      name = "netboot-installer";
      paths =
        with self.nixosConfigurations.netboot-installer.config.system.build; [
          netbootRamdisk
          netbootIpxeScript
          kernel
        ];
    };
  };
}
