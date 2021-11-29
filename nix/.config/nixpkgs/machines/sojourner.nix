{ lib, ... }: {
  config = {
    a3 = {
      enable = true;
      platform = "macOS";
      userName = "aobrien";
    };

    programs.git.signing.signByDefault = lib.mkForce false;
  };
}
