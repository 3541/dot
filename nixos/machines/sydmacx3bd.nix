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
              if [ -f ~/generate_j_aliases.sh ]; then
                  . ~/generate_j_aliases.sh
              fi

              mkdir -p $HOME/.bin_override
              export PATH="$HOME/.bin_override:$PATH"
              if [ ! -e "$HOME/.bin_override/git" ]; then
                  ln -s /usr/bin/git "$HOME/.bin_override/git"
              fi

              export SPINUP_CONFIG="default_configs/roadrunner_syd.sh"
              export ALTERNATE_EDITOR=vi

              if command -v nu 2>&1 > /dev/null; then
                  exec nu
              fi
            '';

            nuExtra = ''
              open ~/.j_aliases | lines |
                  filter { |l| $l | str starts-with "alias" } |
                  each { |l| $l | parse "alias {name}='j to {value}'" | get value | first } |
                  uniq |
                  save ~/.j_completions_nu

              module completions {
                  def "nu-complete j" [] {
                      open ~/.j_completions_nu | lines
                  }

                  export extern "j" [
                      value: string@"nu-complete j"
                  ]
              }

              use completions *
            '';

            ui.fonts = {
              ui.size = 16.0;

              editor = {
                font = "Berkeley Mono";
                size = 15.0;
              };
            };
          };
        };

        users.users.aobrien = {
          home = "/Users/aobrien";
          shell = pkgs.nushell;
        };

        system.keyboard = {
          enableKeyMapping = true;
          remapCapsLockToControl = true;
        };

        services = {
          kwm = { enable = false; };

          khd = {
            enable = false;
            i3Keybindings = true;
          };
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

              matchBlocks = let
                devenv = {
                  hostname = "devenv-aobrien.trading.imc.intra";
                  user = "aobrien";
                };
              in {
                container = {
                  hostname = "sytes173";
                  user = "aobrien";
                  port = 7743;
                  identityFile =
                    "/Users/aobrien/src/docker_spinup/docker/developer_centos7/docker_spinup.private_key.pkcs8.pem";
                };
                devenv-aobrien = devenv;
                "devenv-aobrien.trading.imc-intra" = devenv;
              };
            };

            bash.shellAliases = {
              sp = "$HOME/src/docker_spinup/spinup";
              ss = "$HOME/src/docker_spinup/spinup shell";
            };
          };

          home.packages = with pkgs; [ openssh ];
        };

        homebrew = {
          enable = true;
          brews =
            [ "jdtls" "mvnd" "imc/core/imc-wireshark-dissectors" "openjdk@11" ];
          taps = [
            "homebrew/cask"
            "mvndaemon/homebrew-mvnd"
            {
              name = "imc/core";
              clone_target =
                "https://gitlab.trading.imc.intra/all/homebrew-imc";
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
      };
    })
  ];
}
