with import <nixpkgs> { };

stdenv.mkDerivation rec {
  name = "i3blocks-contrib";
  src = pkgs.fetchFromGitHub {
    owner = "vivien";
    repo = "i3blocks-contrib";
    rev = "1105c0bded5830fa6e2b21cdeb8261b4a1bdde5b";
    sha256 = "1rwyr8ijps4q0dq2bwhjir24ih5yw41mbk2yq7gz6zr1fwrfaipy";
  };
}
