#!/bin/sh

set -e

if which emerge > /dev/null 2>&1; then
    scripts/deps-Gentoo.sh $@
fi
