{ lib, ... }: {
  config = {
    a3 = {
      enable = true;
      platform = "macOS";
      userName = "aobrien";
      fontSize = 16.0;
      shExtra = ''
        export SPINUP_CONFIG="default_config/roadrunner_syd.sh"
        alias imc='ssh sytes172'
      '';
    };

    programs.git.signing.signByDefault = lib.mkForce false;
    programs.git.userEmail = lib.mkForce "alex.obrien@imc.com";
  };
}
