{ self, a3, ... }: {
  system = "x86_64-linux";
  modules = [
    ({ lib, pkgs, ... }: {
      imports = [ a3.nixosModule ./opportunity-hardware.nix ];

      config = {
        a3 = {
          enable = true;
          hostName = "opportunity";
          encryptRoot = true;
          build.distributed = false;

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

        boot = {
          kernelPackages = pkgs.linuxPackages_latest;

          initrd = {
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
                device =
                  "/dev/disk/by-uuid/eee5b77a-8deb-43d8-a0ec-4cc26046dd45";
                keyFile = "/keyfile";
              };

              hdd2 = {
                device =
                  "/dev/disk/by-uuid/237e5dd2-c2bf-4571-895a-aa9930a8b6ef";
                keyFile = "/keyfile";
              };
            };
          };
        };

        # Allow unprivileged access to keyboard in firmware flash mode.
        services = {
          xserver = {
            dpi = 96;
            xkbOptions = "ctrl:nocaps";
          };

          # Keyboard DFU, GMMK Pro and BNMF F62 (respectively).
          udev.extraRules = ''
            SUBSYSTEMS=="usb", ATTRS{idVendor}=="0483", ATTRS{idProduct}=="df11", TAG+="uaccess"
            SUBSYSTEMS=="usb", ATTRS{idVendor}=="03eb", ATTRS{idProduct}=="2ff0", TAG+="uaccess"
          '';

          # pixiecore = let
          #   netboot =
          #     self.nixosConfigurations.netboot-installer.config.system.build;
          # in {
          #   enable = true;
          #   dhcpNoBind = true;
          #   openFirewall = true;
          #   port = 62912;
          #   statusPort = 62912;
          #   kernel = "${netboot.kernel}/bzImage";
          #   initrd = "${netboot.netbootRamdisk}/initrd";
          #   cmdLine = "init=${netboot.toplevel}/init loglevel=4";
          # };
        };

        networking = {
          interfaces.enp0s25.useDHCP = true;
          bridges.br0.interfaces = [ "eth0" ];
          enableIPv6 = false;
          firewall = {
            checkReversePath = lib.mkForce false;
            interfaces.eth0.allowedTCPPorts = [ 80 ];
          };
        };

        home-manager.users.alex.config = {
          xsession.windowManager.i3.config.startup = [{
            command =
              "xrandr --output DP-0 --mode 3840x2160 --output DVI-D-0 --mode 1680x1050 --left-of DP-0";
            always = true;
          }];
          xsession.windowManager.i3.config.keybindings."Mod4+Shift+o" =
            "exec xrandr --output DVI-D-0 --off";

          home.packages = with pkgs; [
            lutris
            prismlauncher
            qmk
            skypeforlinux
            (writeShellScriptBin "me3t" ''
              WINEPREFIX=/mass/games/me3t/wine ${wineWowPackages.staging}/bin/wine64 /mass/games/me3t/ME3TweaksModManager.exe
            '')
            (writeShellScriptBin "me3t7" ''
              WINEPREFIX=/mass/games/me3t/wine ${wineWowPackages.staging}/bin/wine64 /mass/games/me3t/ME3TweaksModManager7.exe
            '')
          ];
        };

        swapDevices = [{ device = "/opt/swapfile"; }];
        programs.steam.enable = true;
      };
    })
  ];
}
