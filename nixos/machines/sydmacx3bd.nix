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
            shExtra = ''
              if [ -f ~/generate_j_aliases.sh ]; then
                  . ~/generate_j_aliases.sh
              fi

              mkdir -p $HOME/.bin_override
              export PATH="$HOME/.bin_override:$PATH"
              if [ ! -e "$HOME/.bin_override/git" ]; then
                  ln -s /usr/bin/git "$HOME/.bin_override/git"
              fi
              if [ ! -e "$HOME/.bin_override/bash" ] && [ -e "$HOME/.nix-profile/bin/bash" ]; then
                  ln -s "$HOME/.nix-profile/bin/bash" "$HOME/.bin_override/bash"
              fi

              export SPINUP_CONFIG="default_configs/roadrunner_syd.sh"
              export ALTERNATE_EDITOR=vi

              # >>> conda initialize >>>
              # !! Contents within this block are managed by 'conda init' !!
              __conda_setup="$('/usr/local/Caskroom/miniconda/base/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
              if [ $? -eq 0 ]; then
                  eval "$__conda_setup"
              else
              if [ -f "/usr/local/Caskroom/miniconda/base/etc/profile.d/conda.sh" ]; then
                  . "/usr/local/Caskroom/miniconda/base/etc/profile.d/conda.sh"
              else
                  export PATH="/usr/local/Caskroom/miniconda/base/bin:$PATH"
              fi
              fi
              unset __conda_setup
              # <<< conda initialize <<<

              if [[ -z "$NU_NESTED" ]]; then
                export NU_NESTED=0
              fi
              if command -v nu 2>&1 > /dev/null && [[ "$NU_NESTED" -lt 2 ]]; then
                  ((++NU_NESTED))
                  exec nu
              fi
            '';

            nuExtra = ''
              open ~/.j_aliases | lines |
                  filter { |l| $l | str starts-with "alias" } |
                  each { |l| $l | parse "alias {name}='j to {value}'" | get value | first } |
                  uniq |
                  save -f ~/.j_completions_nu

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
                  identityFile = "/Users/aobrien/.ssh/id_ed25519";
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

          home.packages = with pkgs; [ openssh bashInteractive pyright ];
        };

        homebrew = {
          enable = true;
          brews = [
            "jdtls"
            "mvnd"
            "imc/core/imc-wireshark-dissectors"
            "openjdk@11"
            "ca-certificates-imc"
            "wireshark"
            "gcc@7"
            "gnu-sed"
          ];
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
            "intellij-idea"
            "clion"
            "eclipse-java"
            "jdk-mission-control"
            "devenv-launcher"
            "xpra"
            "jetbrains-gateway"
            "imc/core/ark"
            "microsoft-remote-desktop"
            "miniconda"
            "tigervnc-viewer"
            "pycharm"
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
