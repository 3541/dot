{ a3, ... }: {
  system = "x86_64-linux";
  modules = [
    ({ lib, pkgs, ... }: {
      imports = [ a3.nixosModule ./opportunity-hardware.nix ];

      config = {
        a3 = {
          enable = true;
          hostName = "opportunity";
          encryptRoot = true;

          boot = {
            loader = "grub";
            esp = "/boot/efi";
          };

          backup = {
            enable = true;
            repo = "marx";
          };

          display = {
            enable = true;
            server = "xorg";
            drivers = [ "nvidia" ];
          };

          home = {
            enable = true;

            ui = {
              windowGaps = true;
              fonts.editor.font = "Berkeley Mono";
            };
          };
        };

        boot.initrd = {
          secrets.keyfile = "/etc/secrets/initrd/keyfile";

          luks.devices = {
            boot.keyFile = "/keyfile";
            hdd0.keyFile = "/keyfile";

            root = {
              allowDiscards = true;
              keyFile = "/keyfile";
            };

            aux = {
              allowDiscards = true;
              keyFile = "/keyfile";
            };

            images = {
              allowDiscards = true;
              keyFile = "/keyfile";
            };

            hdd1 = {
              device = "/dev/disk/by-uuid/eee5b77a-8deb-43d8-a0ec-4cc26046dd45";
              keyFile = "/keyfile";
            };

            hdd2 = {
              device = "/dev/disk/by-uuid/237e5dd2-c2bf-4571-895a-aa9930a8b6ef";
              keyFile = "/keyfile";
            };
          };
        };

        # Allow unprivileged access to keyboard in firmware flash mode.
        services.udev.extraRules = ''
          SUBSYSTEMS=="usb", ATTRS{idVendor}=="0483", ATTRS{idProduct}=="df11", TAG+="uaccess"
        '';

        networking = {
          interfaces.enp0s25.useDHCP = true;
          bridges.br0.interfaces = [ "eth0" ];
          firewall = {
            checkReversePath = lib.mkForce false;
            interfaces.eth0.allowedTCPPorts = [ 80 ];
          };
        };

        home-manager.users.alex.config = {
          xsession.windowManager.i3.config.startup = [{
            command =
              "xrandr --output DP-0 --mode 3840x2160 --output DVI-D-0 --mode 1680x1050 --right-of DP-0";
            always = true;
          }];
          xsession.windowManager.i3.config.keybindings."Mod4+Shift+o" =
            "exec xrandr --output DVI-D-0 --off";

          home.packages = with pkgs; [
            lutris
            polymc
            qmk
            (writeShellScriptBin "me3t" ''
              WINEPREFIX=/mass/games/me3t/wine ${wineWowPackages.base}/bin/wine64 /mass/games/me3t/ME3TweaksModManager.exe
            '')
          ];
        };

        swapDevices = [{ device = "/opt/swapfile"; }];
        services.xserver.dpi = 96;
        programs.steam.enable = true;
      };
    })
  ];
}
