{
  pkgs,
  cfg,
  lib,
  package-inputs,
  ...
}:
let
  pkg = if (cfg.system.os == "darwin") then pkgs.emacsMacport else pkgs.emacs;
in
{
  programs.emacs = {
    enable = true;
    package = pkg;
    extraPackages = epkgs: with epkgs; [ treesit-grammars.with-all-grammars ];
  };

  services.emacs.enable = cfg.system.role == "workstation" && cfg.system.os != "darwin";

  # Manually define the service so that we can override environment variables.
  launchd.agents.emacs = lib.mkIf (cfg.system.role == "workstation" && cfg.system.os == "darwin") {
    enable = true;
    config = {
      ProgramArguments = [
        "${pkg}/bin/emacs"
        "--fg-daemon"
      ];
      EnvironmentVariables.COLORTERM = "truecolor";
      RunAtLoad = true;
      KeepAlive = {
        Crashed = true;
        SuccessfulExit = false;
      };
    };
  };

  home.file = {
    ".emacs.d/init.el".source = pkgs.replaceVars ./init.el {
      font = "${cfg.gui.font.text.family}-${toString cfg.gui.font.text.size}";
      nixfmt = "${pkgs.nixfmt-rfc-style}/bin/nixfmt";
      ispell = "${pkgs.ispell}/bin/ispell";
    };

    # TODO: Use :vcs with use-package once on Emacs >= 30.
    ".emacs.d/sensible-defaults.el".source =
      package-inputs.emacs-sensible-defaults + "/sensible-defaults.el";
  };
}
