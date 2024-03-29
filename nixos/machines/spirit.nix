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
          build.distributed = true;
          fs.tmpOnTmpfs = false;

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

        home-manager.users.alex.config.wayland.windowManager.sway.config = {
          output = {
            "eDP-1" = {
              resolution = "3840x2160";
              scale = "1.0";
            };
            "HDMI-A-1".resolution = "3840x2160";
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
        networking.enableIPv6 = false;
        environment.variables.GDK_DPI_SCALE = "1.5";
        services.xserver.dpi = 96;
        programs.steam.enable = true;
      };
    })
  ];
}
