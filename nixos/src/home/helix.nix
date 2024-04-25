{ cfg, lib, pkgsUnstable, ... }: {
  config.programs.helix =
    lib.mkIf (cfg.enable && cfg.home.enable && cfg.role == "workstation") {
      enable = true;
      package = pkgsUnstable.helix;

      settings = {
        theme = "solarized_dark";

        editor = {
          line-number = "relative";
          cursor-shape.insert = "bar";
          scrolloff = 3;
          cursorline = true;
          true-color = true;
          bufferline = "multiple";
          color-modes = true;
          popup-border = "all";
          lsp.display-inlay-hints = true;

          auto-pairs = {
            "(" = ")";
            "{" = "}";
            "[" = "]";
            "'" = "'";
            "\"" = ''"'';
            "`" = "`";
            "<" = ">";
          };

        };

        keys.normal = {
          "A-." = "goto_definition";
          "A-," = "jump_backward";
          "A->" = "jump_forward";
          "A-m" = "remove_primary_selection";
        };
      };

      languages.language = [{
        name = "nix";
        scope = "source.nix";
        file-types = [ "nix" ];
        formatter.command = "nixfmt";
      }];
    };
}
