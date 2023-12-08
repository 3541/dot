#!/bin/sh

name="$1"
shift
nix run "nixpkgs/nixos-23.11#${name}" -- $@
