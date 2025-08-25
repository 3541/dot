{
  description = "Top-level flake entrypoint for use with nix-darwin.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-25.05-darwin";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    lix = {
      url = "https://git.lix.systems/lix-project/nixos-module/archive/2.93.3-1.tar.gz";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-darwin = {
      url = "github:nix-darwin/nix-darwin/nix-darwin-25.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager/release-25.05";
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
    morlana = {
      url = "github:ryanccn/morlana";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-sensible-defaults = {
      url = "github:hrs/sensible-defaults.el";
      flake = false;
    };
  };

  outputs =
    inputs@{
      self,
      nix-darwin,
      nixpkgs,
      nixpkgs-unstable,
      lix,
      home-manager,
      bash-env-json,
      bash-env-nushell,
      ...
    }:
    let
      system = "aarch64-darwin";
      pkgsUnstable = nixpkgs-unstable.legacyPackages.${system};

      bash-env = bash-env-json.packages.${system}.default;
      bash-env-nu = bash-env-nushell.packages.${system}.default;

      configuration =
        { pkgs, config, ... }:
        {
          nixpkgs.hostPlatform = system;

          system = {
            configurationRevision = self.rev or self.dirtyRev or null;
            stateVersion = 6;
            primaryUser = config.a3.user.name;
          };

          environment.systemPackages = [
            pkgsUnstable.nushell
            bash-env
            inputs.morlana.packages.${system}.morlana
          ];

          a3 = {
            system.os = "darwin";
            nixpkgs-flake = "nixpkgs/nixpkgs-25.05-darwin";

            home.shell.nuExtra = [
              ''
                export-env {
                  if "__NIX_DARWIN_SET_ENVIRONMENT_DONE" not-in $env {
                    use "${bash-env-nu}/bash-env.nu"
                    bash-env ${config.system.build.setEnvironment} | update PATH { $in | split row (char esep) | append $env.PATH | uniq } | load-env
                  }
                }
              ''
            ];
          };
        };

      homeConfiguration =
        { config, package-inputs, ... }:
        let
          cfg = config.a3;
        in
        {
          imports = [ home-manager.darwinModules.home-manager ];

          users.users.${cfg.user.name} = {
            name = cfg.user.name;
            home = cfg.user.home;
          };

          home-manager = {
            useGlobalPkgs = true;
            useUserPackages = true;

            extraSpecialArgs = {
              cfg = config.a3;
              package-inputs = package-inputs;
            };

            users.${cfg.user.name} = {
              home.stateVersion = "25.05";
              imports = [ ../../nix/home ];
            };

          };
        };

      instantiate-config = name: {
        name = name;
        value = nix-darwin.lib.darwinSystem {
          specialArgs = {
            pkgsUnstable = pkgsUnstable;

            package-inputs = {
              helix = inputs.helix.packages.${system}.helix;
              helix-cogs = inputs.helix.packages.${system}.helix-cogs;
              emacs-sensible-defaults = inputs.emacs-sensible-defaults;
            };
          };

          modules = [
            lix.nixosModules.lixFromNixpkgs
            configuration
            (import ../../machines/${name}.nix)
            (import ../../nix)
            homeConfiguration
          ];
        };
      };
      machines = import ./machines.nix;
    in
    {
      darwinConfigurations = builtins.listToAttrs (map instantiate-config machines);
    };
}
