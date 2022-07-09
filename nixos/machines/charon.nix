{ a3, nixpkgs, nixpkgs-unstable, ... }: {
  system = "x86_64-linux";
  modules = [
    ({ config, lib, pkgs, modulesPath, ... }: {
      imports = [
        a3.nixosModule
        (modulesPath + "/installer/sd-card/sd-image-raspberrypi.nix")
        (modulesPath + "/profiles/minimal.nix")
      ];

      config = let
        cfg = config.a3;
        repos = "/home/git";
        unstable-pkgs =
          nixpkgs-unstable.legacyPackages.x86_64-linux.pkgsCross.raspberryPi;
      in {
        # A whole bunch of hacks to fix cross-compilation problems...
        nixpkgs = {
          # Since there's no substitutes available anyway, there's no harm in setting more specific
          # compiler flags.
          crossSystem = lib.systems.examples.raspberryPi // {
            gcc = {
              cpu = "arm1176jzf-s";
              fpu = "vfp";
            };
          };

          config.packageOverrides = pkgs: {
            # https://github.com/NixOS/nixpkgs/commit/2294dace6ae30d095719a6dd8413242ec61ca8e5.
            btrfs-progs = unstable-pkgs.btrfs-progs;
            # Can be removed once https://github.com/NixOS/nixpkgs/pull/174612/files is in nixpkgs.
            tailscale = unstable-pkgs.tailscale;
            powerline-go = unstable-pkgs.powerline-go;

            # Cross-compilation of Git with Perl support is broken. Thus, Gitweb does not
            # cross-compile. For now, use cgit instead.
            cgit = (pkgs.cgit.override {
              # Cross-compilation of luajit requires that the build and host platforms have the same
              # pointer size. This seems unlikely to be fixed upstream, so the only real resolution
              # is probably to wait for Git+Perl cross-compilation to be fixed in nixpkgs.
              luajit =
                nixpkgs.legacyPackages.i686-linux.pkgsCross.raspberryPi.luajit;
            }).overrideAttrs (old: {
              # Not sure if this is because of an issue with the cgit package or a fundamental
              # nixpkgs cross-compilation problem, but the Python filters all come out with the
              # wrong shebang.
              postInstall = old.postInstall + ''
                echo "Fixing Python shebangs for cross..."
                find "$out/lib/cgit/filters" -type f -perm -0100 -print0 | while read -d "" f; do
                  shebang=$(grep '#!.*bin/python' "$f" || true)
                  if [ -z "$shebang" ]; then
                    continue
                  fi

                  sed -i "s,''${shebang},#!${pkgs.python3}/bin/python3," "$f"
                  echo "Replaced \"$shebang\" with \"${pkgs.python3}/bin/python3\"."
                done
              '';
            });
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
          home.enable = true;
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
        security.sudo.wheelNeedsPassword = false;
        documentation.info.enable = false;
        networking.firewall.allowedTCPPorts = [ 80 443 ];

        boot = {
          supportedFilesystems = lib.mkForce [ "ext4" ];
          initrd.supportedFilesystems = lib.mkForce [ "ext4" ];
        };

        fileSystems = {
          ${repos} = {
            device = "/dev/disk/by-label/git";
            fsType = "ext4";
          };

          "/persist" = {
            device = "/dev/disk/by-label/persist";
            fsType = "ext4";
          };
        };

        users = {
          mutableUsers = false;

          users.git = {
            isNormalUser = true;
            description = "Git SSH user.";
            home = repos;
            createHome = true;
            shell = "${pkgs.git}/bin/git-shell";
            openssh.authorizedKeys.keys = cfg.user.authorizedKeys;
          };
        };

        programs.git = {
          enable = true;
          config.init.defaultBranch = "trunk";
        };

        systemd.services.tailscale-up = {
          description = "Launch and authenticate Tailscale.";
          after = [ "tailscaled.service" ];
          wantedBy = [ "multi-user.target" ];
          unitConfig.ConditionPathExists = "!/tailscale-registered";
          serviceConfig.Type = "oneshot";
          script = ''
            ${pkgs.tailscale}/bin/tailscale up --auth-key file:/persist/tailscale-auth
            touch /tailscale-registered
          '';
        };

        services = {
          udisks2.enable = false;

          # gitweb = {
          #   projectroot = repos;

          #   extraConfig = ''
          #     $feature{'pathinfo'}{'default'} = [1];
          #     $feature{'timed'}{'default'} = [1];
          #   '';
          # };

          # nginx = {
          #   enable = true;
          #   recommendedOptimisation = true;
          #   recommendedTlsSettings = true;
          #   recommendedGzipSettings = true;
          #   recommendedProxySettings = true;
          #   virtualHosts."_".kTLS = true;

          #   gitweb = {
          #     enable = true;
          #     location = "/git";
          #   };
          # };

          lighttpd = {
            enable = true;

            cgit = {
              enable = true;
              subdir = "";

              # Note: source-filter must be defined before scan-path.
              configText = ''
                source-filter=${pkgs.cgit}/lib/cgit/filters/syntax-highlighting.py
                enable-index-links=1
                enable-log-filecount=1
                enable-log-linecount=1
                enable-git-config=1
                cache-size=1000
                root-title=git.3541.website
                root-desc=
                readme=README.html
                scan-path=${repos}
              '';
            };
          };
        };

        environment.systemPackages = with pkgs;
          let
            repo-mirror-push = pkgs.writeShellScriptBin "repo-mirror-push" ''
              set -e

              echo "Pushing to mirror..."
              GIT_SSH_COMMAND="ssh -i /persist/id_ed25519 -o IdentitiesOnly=yes" \
                git push --mirror mirror
            '';
            hook-post-update = pkgs.writeShellScriptBin "hook-post-update" ''
              set -e

              git update-server-info

              # cgit can render markdown on request, but it is incredibly slow to do so.
              if git cat-file -e HEAD:README.md > /dev/null 2>&1; then
                git cat-file blob HEAD:README.md | ${pkgs.cmark}/bin/cmark > README.html
              fi

              if git remote get-url mirror > /dev/null 2>&1; then
                ${repo-mirror-push}/bin/repo-mirror-push
              fi
            '';
            repo-new = pkgs.writeShellScriptBin "repo-new" ''
              set -e

              if [ -z "${repos}" ] || [ ! -d "${repos}" ]; then
                echo "Repository directory \"${repos}\" is missing or inaccessible."
                exit 1
              fi

              if [ -z "''${1-}" ]; then
                echo "Repository name must be specified."
                exit 1
              fi

              repo="${repos}/$1.git"
              if [ -e "$repo" ]; then
                echo "Repository \"$repo\" already exists."
                exit 1
              fi

              sudo -u git mkdir -p "$repo"
              sudo -u git ${pkgs.git}/bin/git init --bare "$repo"
              sudo -u git ${pkgs.git}/bin/git -C "$repo" config gitweb.owner "3541@3541.website"

              echo "Writing post-update hook... "
              cat <<- EOF | sudo -u git tee "$repo/hooks/post-update"
                #!/bin/sh
                set -e

                exec hook-post-update
              EOF
              sudo -u git chmod +x "$repo/hooks/post-update"
            '';
            repo-mirror-init = pkgs.writeShellScriptBin "repo-mirror-init" ''
              set -e

              if [ -z "''${1-}" ]; then
                echo "Repository name must be specified."
                exit 1
              fi

              if [ -z "''${2-}" ]; then
                echo "Mirror URL must be specified."
                exit 1
              fi

              repo="${repos}/$1.git"
              if [ ! -d "$repo" ]; then
                echo "Repository \"$repo\" does not exist."
                exit 1
              fi

              if ! git remote get-url mirror > /dev/null 2>&1; then
                sudo -u git git -C "$repo" remote add mirror "$2"
              fi
            '';
          in [ hook-post-update repo-new repo-mirror-push repo-mirror-init ];
      };
    })
  ];
}
