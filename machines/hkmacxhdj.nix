{ ... }:
{
  a3 = {
    user = {
      name = "aobrien";
      home = "/Users/aobrien";
      email = "alex.obrien@imc.com";
    };

    gui.font = {
      text.size = 14;
      ui.size = 14;
    };
  };

  security.pki.certificateFiles = [ "/opt/homebrew/etc/ca-certificates-imc/cert.pem" ];
}
