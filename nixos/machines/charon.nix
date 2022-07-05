{ a3, nixpkgs-unstable, ... }: {
  system = "x86_64-linux";
  modules = [
    ({ config, lib, pkgs, modulesPath, ... }: {
      imports = [
        a3.nixosModule
        (modulesPath + "/installer/sd-card/sd-image-raspberrypi.nix")
      ];

      config = let
        cfg = config.a3;
        repos = "/home/git";
        unstable-pkgs =
          nixpkgs-unstable.legacyPackages.x86_64-linux.pkgsCross.raspberryPi;
      in {
        nixpkgs = {
          crossSystem = lib.systems.examples.raspberryPi;

          config.packageOverrides = pkgs: {
            # https://github.com/NixOS/nixpkgs/commit/2294dace6ae30d095719a6dd8413242ec61ca8e5.
            btrfs-progs = unstable-pkgs.btrfs-progs;
            # Can be removed once https://github.com/NixOS/nixpkgs/pull/174612/files is in nixpkgs.
            tailscale = unstable-pkgs.tailscale;
            powerline-go = unstable-pkgs.powerline-go;

            openldap = pkgs.openldap.overrideAttrs
              (final: prev: { enableParallelBuilding = false; });
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
        services.udisks2.enable = false;
        documentation.info.enable = false;

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
          serviceConfig.Type = "oneshot";
          script = ''
            ${pkgs.tailscale}/bin/tailscale up --auth-key file:/persist/tailscale-auth
          '';
        };

        environment.systemPackages = with pkgs;
          let
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

              echo "Writing post-update hook... "
              cat <<- EOF | sudo -u git tee "$repo/hooks/post-update"
                #!/bin/sh
                set -e

                git update-server-info
              EOF
              sudo -u git chmod +x "$repo/hooks/post-update"
            '';
            repo-mirror-push = pkgs.writeShellScriptBin "repo-mirror-push" ''
              set -e

              GIT_SSH_COMMAND="ssh -i /persist/id_ed25519 -o IdentitiesOnly=yes" \
                git push --mirror mirror
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

              hook="$repo/hooks/post-update"
              if [ -e "$hook" ] && grep -q repo-mirror-push "$hook"; then
                echo "Post-update hook already added."
                exit 1
              fi

              if [ ! -f "$hook" ]; then
                echo "Post-update hook not present."
                exit 1
              fi

              echo "Updating post-update hook... "
              cat <<- EOF | sudo -u git tee -a "$hook"
                echo "Mirroring to \$(git remote get-url mirror)..."
                "${repo-mirror-push}"/bin/repo-mirror-push
              EOF
            '';
          in [ repo-new repo-mirror-push repo-mirror-init ];
      };
    })
  ];
}
