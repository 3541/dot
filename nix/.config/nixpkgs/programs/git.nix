{ config, pkgs, ... }: {
  programs.git = {
    enable = true;
    userName = "Alex O'Brien";
    userEmail = "3541ax@gmail.com";

    signing = {
      key = "0x1EECFF9EE39ED7AA";
      signByDefault = true;
    };

    delta = {
      enable = true;
      options = {
        features = "side-by-side line-numbers";
        syntax-theme = "Solarized (dark)";
        whitespace-error-style = "22 reverse";
      };
    };

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

  programs.gh.enable = true;
}
