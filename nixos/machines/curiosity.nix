{ a3, apple, ... }: {
  system = "aarch64-linux";
  modules = [
    ({ lib, pkgs, modulesPath, ... }: {
      imports =
        [ apple.nixosModules.default a3.nixosModule ./curiosity-hardware.nix ];

      config = {
        a3 = {
          enable = true;
          hostName = "curiosity";
          encryptRoot = true;
          role = "workstation";
          fs.tmpOnTmpfs = false;
          boot.canTouchEfiVariables = false;
          system = "aarch64-linux";

          hardware = {
            formFactor = "portable";
            cpu = "arm";
          };

          display = {
            enable = true;
            hidpi = true;
            drivers = [ "modesetting" ];
          };

          home = {
            enable = true;

            ui.fonts = {
              ui = {
                font = "Berkeley Mono";
                size = 9.0;
              };

              editor = {
                font = "Berkeley Mono";
                size = 10.0;
              };
            };
          };
        };

        hardware = {
          pulseaudio.enable = lib.mkForce false;

          asahi = {
            useExperimentalGPUDriver = true;
            experimentalGPUInstallMode = "replace";
            peripheralFirmwareDirectory = ./firmware;
          };
        };

        networking.wireless.iwd = {
          enable = true;
          settings.General.EnableNetworkConfiguration = true;
        };

        environment.sessionVariables.MOZ_GMP_PATH = (let
          lacrosVersion = "120.0.6098.0";
          widevine-installer = pkgs.stdenv.mkDerivation rec {
            name = "widevine-installer";
            version = "7a3928fe1342fb07d96f61c2b094e3287588958b";
            src = pkgs.fetchFromGitHub {
              owner = "AsahiLinux";
              repo = "${name}";
              rev = "${version}";
              sha256 = "sha256-XI1y4pVNpXS+jqFs0KyVMrxcULOJ5rADsgvwfLF6e0Y=";
            };

            buildInputs = with pkgs; [ which python3 squashfsTools ];

            installPhase = ''
              mkdir -p "$out/bin"
              cp widevine-installer "$out/bin/"
              cp widevine_fixup.py "$out/bin/"
              echo "$(which unsquashfs)"
              sed -e "s|unsquashfs|$(which unsquashfs)|" -i "$out/bin/widevine-installer"
              sed -e "s|python3|$(which python3)|" -i "$out/bin/widevine-installer"
              sed -e "s|read|#read|" -i "$out/bin/widevine-installer"
              sed -e 's|$(whoami)|root|' -i "$out/bin/widevine-installer"
              sed -e 's|URL=.*|URL="$DISTFILES_BASE"|' -i "$out/bin/widevine-installer"
            '';
          };
          widevine = pkgs.stdenv.mkDerivation {
            name = "widevine";
            version = "";
            buildInputs = with pkgs; [ curl widevine-installer ];

            src = pkgs.fetchurl {
              urls = [
                "https://commondatastorage.googleapis.com/chromeos-localmirror/distfiles/chromeos-lacros-arm64-squash-zstd-${lacrosVersion}"
              ];
              hash = "sha256-OKV8w5da9oZ1oSGbADVPCIkP9Y0MVLaQ3PXS3ZBLFXY=";
            };

            unpackPhase = "true";
            installPhase = ''
              mkdir -p "$out/"
              COPY_CONFIGS=0 INSTALL_BASE="$out" DISTFILES_BASE="file://$src" widevine-installer
            '';
          };
        in "${widevine}/gmp-widevinecdm/system-installed");

        home-manager.users.alex.config = {
          programs.git.signing.signByDefault = lib.mkForce false;

          wayland.windowManager.sway.config.input = {
            "type:keyboard".xkb_options = "ctrl:nocaps";

            "type:touchpad" = {
              natural_scroll = "disabled";
              pointer_accel = "0.5";
            };
          };
        };
      };
    })
  ];
}
