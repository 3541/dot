{ a3, nixos-hardware, ... }: {
  system = "x86_64-linux";
  modules = [
    ({ lib, pkgs, ... }: {
      imports = [
        a3.nixosModule
        ./spirit-hardware.nix
        nixos-hardware.nixosModules.lenovo-thinkpad-x1-extreme
      ];

      config = {
        a3 = {
          enable = true;
          hostName = "spirit";
          encryptRoot = true;
          build.sshKey = "id_ecdsa";
          hardware.formFactor = "portable";
          backup.enable = true;

          display = {
            enable = true;
            hidpi = true;
            drivers = [ "modesetting" "nouveau" ];
          };

          home = {
            enable = true;

            ui.fonts = {
              ui.size = 16.0;
              editor = {
                font = "Berkeley Mono";
                size = 15.0;
              };
            };
          };
        };

        services.undervolt = {
          enable = true;
          coreOffset = -150;
          uncoreOffset = -150;
        };

        hardware.trackpoint = {
          enable = true;
          device = "TPPS/2 Elan TrackPoint";
          speed = 255;
          sensitivity = 255;
        };

        # Something goes wrong when building bcachefs as a patch on top of upstream 5.18. As such,
        # it is currently marked broken in nixpkgs. Once 5.19 arrives and the bcachefs tree updates,
        # this may be fixed.
        boot.kernelPackages = lib.mkForce (let
          commit = "8b687170aabd3de7be3ea6b75702d60fbc204abc";
          version = "5.18";
          kernelPackage = { buildLinux, ... }@args:
            buildLinux (args // {
              version = "${version}-bcachefs-${commit}";
              modDirVersion = "${version}.0";
              extraMeta.branch = "master";
              extraConfig = "BCACHEFS_FS y";
              kernelPatches = [];

              src = pkgs.fetchgit {
                url = "https://evilpiepirate.org/git/bcachefs.git";
                rev = commit;
                sha256 = "NE3qTWsFAAWtphQNYztrvhMTilDpsag2osLxyo0fhTE=";
              };
            } // (args.argsOverride or { }));
        in pkgs.linuxPackagesFor (pkgs.callPackage kernelPackage { }));

        home-manager.users.alex.config.wayland.windowManager.sway.config = {
          output = {
            "eDP-1" = {
              resolution = "3840x2160";
              scale = "1.0";
            };
            "HDMI-A-1".resolution = "1920x1080";
          };

          input = {
            "type:touchpad".events = "disabled";
            "type:touch".events = "disabled";
            "type:tablet tool".events = "disabled";
            "type:keyboard".xkb_options = "ctrl:nocaps";
          };
        };

        networking.interfaces.enp0s31f6.useDHCP = true;
        networking.interfaces.wlp0s20f3.useDHCP = true;
        environment.variables.GDK_DPI_SCALE = "1.5";
        services.xserver.dpi = 96;
        programs.steam.enable = true;
      };
    })
  ];
}
