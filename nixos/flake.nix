{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-22.05";
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    # Old nixpkgs with kernel 5.17.14 for bcachefs.
    nixpkgs-bcachefs.url = "nixpkgs/efb6eb853e8b1546ad7760a31159bf1a8ea132b3";

    a3 = {
      url = "./src";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, ... }@args: {
    nixosConfigurations = builtins.listToAttrs (map (name: {
      name = name;
      value = nixpkgs.lib.nixosSystem
        ((import ./machines/${name}.nix args) // { specialArgs = args; });
    }) [ "opportunity" ]);
  };
}
