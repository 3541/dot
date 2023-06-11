{ a3, ... }: {
  system = "x86_64-linux";
  modules = [
    ({ pkgs, modulesPath, ... }: {
      imports = [
        (modulesPath + "/profiles/minimal.nix")
        (modulesPath + "/virtualisation/proxmox-lxc.nix")
        a3.nixosModule
      ];

      config = {
        a3 = {
          enable = true;
          hostName = "nixos-template-please-rename";
          encryptRoot = false;
          role = "server";
          minimal = true;
          fs.tmpOnTmpfs = false;
          home.enable = true;
          boot.enable = false;
        };

        environment.systemPackages = [
          (pkgs.writeShellScriptBin "instantiate-template" ''
            set -euo pipefail

            HOSTNAME="$(hostname)"

            rm -r ~/.config/nushell

            ${pkgs.git}/bin/git clone https://github.com/3541/dot.git
            cd dot
            ./install.sh
            cp nixos/machines/lxc-template.nix "nixos/machines/$HOSTNAME.nix"
            sed -i "s/hostName = \"nixos-template-please-rename\"/hostName = \"$HOSTNAME\"/" "nixos/machines/$HOSTNAME.nix"
            sed -i "s/# Add machines here./\"$HOSTNAME\"/" nixos/flake.nix
            git add -A
            git config user.name "Alex O'Brien"
            git config user.email "3541@3541.website"
            git commit --no-verify -m "Add template instantiation $HOSTNAME."
            sudo nixos-rebuild switch --flake "$PWD/nixos#$HOSTNAME"
          '')
        ];
      };
    })
  ];
}
