{ lib, config, pkgs, ... }:
let cfg = config.a3;
in {
  programs.git = {
    enable = true;
    userName = "Alex O'Brien";
    userEmail = "3541ax@gmail.com";

    signing = lib.mkIf (cfg.role == "workstation") {
      key = "0x1EECFF9EE39ED7AA";
      signByDefault = true;
    };

    delta = lib.mkIf (cfg.role == "workstation") {
      enable = true;
      options = {
        features = "side-by-side line-numbers";
        syntax-theme = "Solarized (dark)";
        whitespace-error-style = "22 reverse";
      };
    };

    ignores = [ ".DS_Store" ];

    extraConfig = {
      submodule.fetchJobs = 8;
      pull = {
        rebase = false;
        ff = "only";
      };
      log.showSignature = true;
      init.defaultBranch = "trunk";
    };
  };

  programs.gh.enable = cfg.role == "workstation";
}
