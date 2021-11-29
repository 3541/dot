{ lib, ... }: {
  config = {
    a3 = {
      enable = true;
      platform = "macOS";
      userName = "aobrien";
      fontSize = 16.0;
    };

    programs.git.signing.signByDefault = lib.mkForce false;
  };
}
