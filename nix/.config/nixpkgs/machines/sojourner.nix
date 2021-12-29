{ lib, pkgs, ... }: {
  config = {
    a3 = {
      enable = true;
      platform = "macOS";
      userName = "aobrien";
      fontSize = 16.0;
      shExtra = ''
        export PATH="$PATH:/opt/local/bin"
        export SPINUP_CONFIG="default_config/roadrunner_syd.sh"
        alias imc='ssh -Y sytes172'
        alias imcb='ssh sydtl003'
        alias ss='~/src/docker_spinup/spinup shell'
        alias sst='~/src/docker_spinup/spinup stop'

        if [[ -f ~/generate_j_aliases.sh ]]; then
            . ~/generate_j_aliases.sh
        fi
      '';
    };

    programs.git.signing.signByDefault = lib.mkForce false;
    programs.git.userEmail = lib.mkForce "alex.obrien@imc.com";

    home.packages = [
      (pkgs.writeShellScriptBin "clangd" ''
        /usr/local/bin/docker exec -i aobrien_v2_dev_container /opt/imc/llvm-11.0.0/bin/clangd --path-mappings=/Users/aobrien=/outside ''${@:1}
      '')
    ];
  };
}
