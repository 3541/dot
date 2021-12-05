{ lib, pkgs, ... }: {
  config = {
    a3 = {
      enable = true;
      platform = "macOS";
      userName = "aobrien";
      fontSize = 16.0;
      shExtra = ''
        export SPINUP_CONFIG="default_config/roadrunner_syd.sh"
        alias imc='ssh -X sytes172'
        alias ss='~/src/docker_spinup/spinup shell'
        alias sst='~/src/docker_spinup/spinup stop'
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
