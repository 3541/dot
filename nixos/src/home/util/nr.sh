#!/bin/sh

name="$1"
shift
nix run "nixpkgs/nixos-22.05#${name}" -- $@
