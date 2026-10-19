{
  description = "Top-level flake entrypoint for use with home-manager.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    lix = {
      url = "https://git.lix.systems/lix-project/nixos-module/archive/c47f62187601ea2991b79a9bacdbfdf76cd29fbe.tar.gz";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager/release-26.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    bash-env-json = {
      url = "github:tesujimath/bash-env-json/main";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    bash-env-nushell = {
      url = "github:tesujimath/bash-env-nushell/main";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.bash-env-json.follows = "bash-env-json";
    };

    helix = {
      url = "github:3541/helix/patched";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-sensible-defaults = {
      url = "github:hrs/sensible-defaults.el";
      flake = false;
    };
  };

  outputs =
    inputs@{
      nixpkgs,
      nixpkgs-unstable,
      lix,
      home-manager,
      bash-env-nushell,
      ...
    }:
    let
      # TODO: other architectures...?
      system = "x86_64-linux";
      lixOverlay = lix.overlays.lixFromNixpkgs;

      makeNixpkgs = nixpkgs: nixpkgs.legacyPackages.${system}.extend lixOverlay;

      pkgs = makeNixpkgs nixpkgs;
      pkgsUnstable = makeNixpkgs nixpkgs-unstable;

      bash-env-nu = bash-env-nushell.packages.${system}.default;

      configuration = {config, ...}: {
        home.stateVersion = "25.11";

        a3 = {
          system.os = "linux";
          orchestrator = "home-manager";
          nixpkgs-flake = "nixos-26.05";

          home.shell.nuExtra = [
            ''
              export-env {
                if "__NIX_SET_ENVIRONMENT_DONE" not-in $env {
                  use "${bash-env-nu}/bash-env.nu"
                  bash-env /etc/profile.d/nix.sh | update PATH { $in | split row (char esep) | append $env.PATH | uniq } | load-env
                  $env.__NIX_SET_ENVIRONMENT_DONE = 1
                }
              }
            ''
          ];
        };

        _module.args.cfg = config.a3;
        imports = [ ../../nix/home ];
      };

      instantiate-config = name: {
        name = name;
        value = home-manager.lib.homeManagerConfiguration {
          inherit pkgs;

          extraSpecialArgs = {
            inherit pkgsUnstable;

            package-inputs = {
              helix = inputs.helix.packages.${system}.helix;
              helix-cogs = inputs.helix.packages.${system}.helix-cogs;
              emacs-sensible-defaults = inputs.emacs-sensible-defaults;
            };
          };

          modules = [
            configuration
            ../../machines/${name}.nix
            ../../nix
            ../../nix/home/options.nix
          ];
        };
      };
      machines = import ./machines.nix;
    in
    {
      homeConfigurations = builtins.listToAttrs (map instantiate-config machines);
    };
}
