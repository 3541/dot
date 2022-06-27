{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-22.05";
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    # Old nixpkgs with kernel 5.17.14 for bcachefs. See machines/spirit.nix.
    nixpkgs-bcachefs.url = "nixpkgs/efb6eb853e8b1546ad7760a31159bf1a8ea132b3";
    # Newer nixpkgs with setuptools updated for cross compilation. Also used to get fixed Go
    # packages. See machines/charon.nix.
    nixpkgs-setuptools.url = "nixpkgs/2294dace6ae30d095719a6dd8413242ec61ca8e5";

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
    }) [ "opportunity" "spirit" "charon" ]);

    packages.x86_64-linux.charonImage =
      self.nixosConfigurations.charon.config.system.build.sdImage;
  };
}
