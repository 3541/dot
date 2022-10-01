{ self, a3, ... }: {
  system = "x86_64-darwin";

  modules = [
    ({ lib, pkgs, ... }: {
      imports = [ a3.darwinModule ];

      config = {
        a3 = {
          enable = true;
          hostName = "sydmacx3bd";
          platform = "darwin";
          hardware.formFactor = "portable";

          home = {
            enable = true;
            user = "aobrien";
            directory = "/Users/aobrien";
            #            shExtra = "export JAVA_HOME=$(/usr/libexec/java_home –v 17)";
            shExtra = ''
              # Generate J Aliases
              if [ -f ~/generate_j_aliases.sh ]; then
                  . ~/generate_j_aliases.sh
              fi

              mkdir -p $HOME/.bin_override
              export PATH="$HOME/.bin_override:$PATH"
              if [ ! -e "$HOME/.bin_override/git" ]; then
                  ln -s /usr/bin/git "$HOME/.bin_override/git"
              fi

              export ALTERNATE_EDITOR=vi
            '';

            ui.fonts = {
              ui.size = 16.0;

              editor = {
                font = "Berkeley Mono";
                size = 13.0;
              };
            };
          };
        };

        users.users.aobrien = {
          home = "/Users/aobrien";
          shell = pkgs.bashInteractive;
        };

        system.keyboard = {
          enableKeyMapping = true;
          remapCapsLockToControl = true;
        };

        home-manager.users.aobrien.config = {
          programs = {
            git = {
              userEmail = lib.mkForce "alex.obrien@imc.com";
              signing.signByDefault = lib.mkForce false;
              extraConfig.http.emptyAuth = true;
            };

            ssh = {
              extraConfig = ''
                PreferredAuthentications gssapi-with-mic,publickey,password,keyboard-interactive
                GSSAPIAuthentication yes
                GSSAPIDelegateCredentials yes
              '';

              matchBlocks = let dsConfig = {
                hostname = "aobrien-devschool.trading.imc.intra";
                user = "aobrien";
                identityFile = "/Users/aobrien/.ssh/devenv_private_key";
                checkHostIP = false;
                # compression = true;

                extraOptions.StrictHostKeyChecking = "no";
              }; in {
                devschool = dsConfig;
                "devenv-aobrien.trading.imc.intra" = dsConfig;
              };
            };

            bash.shellAliases = {
              sp = "$HOME/src/docker_spinup/spinup";
              ss = "$HOME/src/docker_spinup/spinup shell";
            };
          };

          home.packages = [ pkgs.openssh ];
        };

        homebrew = {
          enable = true;
          brews = [ "jdtls" "mvnd" ];
          taps = [
            "homebrew/cask"
            "mvndaemon/homebrew-mvnd"
            {
              name = "imc/core";
              clone_target = "https://gitlab.trading.imc.intra/all/homebrew-imc";
            }
          ];
          casks = [
            "firefox"
            "intellij-idea-ce"
            "clion"
            "eclipse-java"
            "jdk-mission-control"
            "devenv-launcher"
            "tigervnc-viewer-imc"
            "xpra"
            "wireshark"
            "jetbrains-gateway"
            "imc/core/ark"
          ];

          onActivation = {
            autoUpdate = true;
            upgrade = true;
            cleanup = "zap";
          };
        };

        time.timeZone = lib.mkForce "America/Chicago";
      };
    })
  ];
}
