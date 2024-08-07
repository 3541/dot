{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-23.11";
    nixos-hardware.url = "github:NixOS/nixos-hardware";

    darwin = {
      url = "github:lnl7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    a3 = {
      url = "path:./src";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    apple = {
      url = "github:tpwrules/nixos-apple-silicon";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, darwin, ... }@args: {
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
      "lxc-template"
      "curiosity"
      # Add machines here.
    ]);

    darwinConfigurations = builtins.listToAttrs (map (name: {
      name = name;
      value = darwin.lib.darwinSystem
        ((import ./machines/${name}.nix args) // { specialArgs = args; });
    }) [
      "sydmacx3bd"
      "sydmacxhdj"
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
