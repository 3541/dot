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

          backup = {
            enable = true;
            repo = "marx";
          };

          display = {
            enable = true;
            hidpi = true;
            server = "xorg";
            drivers = [ "modesetting" ];
          };

          home = {
            enable = true;

            ui = {
              windowGaps = true;
              fonts.editor.font = "Berkeley Mono";
              fonts.ui.font = "Berkeley Mono";
            };
          };
        };

        boot.initrd.kernelModules = [ "amdgpu" ];

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

          extraHosts = ''
            192.168.0.123 mercury mercury.local
          '';
        };

        home-manager.users.alex.config = {
          xsession.windowManager.i3.config.startup = [{
            command =
              "xrandr --output DP-2 --mode 3840x2160 --output HDMI-1 --mode 1680x1050 --left-of DP-2";
            always = true;
          }];
          xsession.windowManager.i3.config.keybindings."Mod4+Shift+o" =
            "exec xrandr --output HDMI-1 --off";
          # wayland.windowManager.sway.config = {
          #   input."type:keyboard".xkb_options = "ctrl:nocaps";

          #   output = {
          #     "HDMI-A-1".pos = "0 700";
          #     "DP-2".pos = "1680 0";
          #   };
          # };

          home.packages = with pkgs; [
            lutris
            prismlauncher
            skypeforlinux
            dolphin-emu
            nixos-generators
            gamescope
            direnv
            (mathematica.override {
              source = pkgs.requireFile {
                name = "Mathematica_13.2.1_BNDL_LINUX.sh";
                hashMode = "recursive";
                sha256 = "070ybhgskk3fw8c6fgqs4lq9252ds6585cqdd5as94hj55vjibmq";
                message = "Missing Mathematica installer.";
              };
            })
            (writeShellScriptBin "me3t" ''
              WINEPREFIX=/mass/games/me3t/wine ${wineWowPackages.staging}/bin/wine64 /mass/games/me3t/ME3TweaksModManager.exe
            '')
            (writeShellScriptBin "me3t7" ''
              WINEPREFIX=/mass/games/me3t/wine ${wineWowPackages.staging}/bin/wine64 /mass/games/me3t/ME3TweaksModManager7.exe
            '')
          ];

          programs.ssh.matchBlocks = {
            "*".identityFile = "~/.ssh/id_ed25519_sk_rk";
            "192.168.122.*".identityFile = "~/.ssh/id_ed25519";
          };
        };

        swapDevices = [{ device = "/opt/swapfile"; }];
        programs.steam.enable = true;
      };
    })
  ];
}
