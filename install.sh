#!/bin/sh

set -e

cd $(dirname $(readlink -f "$0"))

for d in bash emacs i3 nvim sakura redshift git; do
    echo "$d"
    stow "$d"
done
