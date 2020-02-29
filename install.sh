#!/bin/sh

cd $(dirname $(readlink -f "$0"))

for d in bash emacs i3 nvim sakura; do
    echo "$d"
    stow "$d"
done
