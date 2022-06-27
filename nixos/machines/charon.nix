{ a3, nixpkgs-setuptools, ... }: {
  system = "x86_64-linux";
  modules = [
    ({ lib, pkgs, modulesPath, ... }: {
      imports = [
        a3.nixosModule
        (modulesPath + "/installer/sd-card/sd-image-raspberrypi.nix")
      ];

      config = {
        nixpkgs = {
          crossSystem = lib.systems.examples.raspberryPi;

          config.packageOverrides = pkgs: {
            openldap = pkgs.openldap.overrideAttrs
              (final: prev: { enableParallelBuilding = false; });

            btrfs-progs =
              nixpkgs-setuptools.legacyPackages.x86_64-linux.btrfs-progs;

            # Can be removed once https://github.com/NixOS/nixpkgs/pull/174612/files is in nixpkgs.
            tailscale =
              nixpkgs-setuptools.legacyPackages.x86_64-linux.tailscale;
            minica = nixpkgs-setuptools.legacyPackages.x86_64-linux.minica;
            lego = nixpkgs-setuptools.legacyPackages.x86_64-linux.lego;
            fail2ban = nixpkgs-setuptools.legacyPackages.x86_64-linux.fail2ban;
          };

          # https://github.com/NixOS/nixpkgs/issues/154163.
          overlays = [
            (final: super: {
              makeModulesClosure = x:
                super.makeModulesClosure (x // { allowMissing = true; });
            })
          ];
        };

        a3 = {
          enable = true;
          hostName = "charon";
          encryptRoot = false;
          role = "server";
          hardware.cpu = "arm";

          boot = {
            method = "other";
            loader = "other";
          };
        };

        system.autoUpgrade.enable = lib.mkForce false;
        users.mutableUsers = false;
        security.sudo.wheelNeedsPassword = false;
      };
    })
  ];
}
