{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-23.05";
    nixpkgs-unstable.url = "nixpkgs/nixpkgs-unstable";
    nixos-hardware.url = "github:NixOS/nixos-hardware";

    darwin = {
      url = "github:lnl7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    a3 = {
      url = "./src";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, nixpkgs-unstable, darwin, ... }@args: {
    nixosConfigurations = builtins.listToAttrs (map (name: {
      name = name;
      value = nixpkgs.lib.nixosSystem
        ((import ./machines/${name}.nix args) // { specialArgs = args; });
    }) [
      "opportunity"
      "spirit"
      "charon"
      "sagittarius"
      "netboot-installer"
      "iso-installer"
      # Add machines here.
    ]);

    darwinConfigurations = builtins.listToAttrs (map (name: {
      name = name;
      value = darwin.lib.darwinSystem
        ((import ./machines/${name}.nix args) // { specialArgs = args; });
    }) [
      "sydmacx3bd"
      # Add macOS machines here.
    ]);

    packages.x86_64-linux = {
      charonImage = self.nixosConfigurations.charon.config.system.build.sdImage;
      pxeInstallImage = nixpkgs.legacyPackages.x86_64-linux.pkgs.symlinkJoin {
        name = "netboot-installer";
        paths =
          with self.nixosConfigurations.netboot-installer.config.system.build; [
            netbootRamdisk
            netbootIpxeScript
            kernel
          ];
      };
      cdInstallImage =
        self.nixosConfigurations.iso-installer.config.system.build.isoImage;
    };
  };
}
