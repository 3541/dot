#!/bin/sh

set -e

if ! which $@ > /dev/null 2>&1; then
    doas pkg_add -u
    doas pkg_add $@
fi
