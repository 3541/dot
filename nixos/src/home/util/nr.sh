#!/bin/sh

name="$1"
shift
nix run "nixpkgs/nixos-22.11#${name}" -- $@
