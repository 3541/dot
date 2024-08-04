#!/bin/sh

set -e

if ! which $@ > /dev/null 2>&1; then
    sudo emerge -a $@
fi
