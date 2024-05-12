#!/bin/sh

if [ ! -f /etc/doas.conf ]; then
    su root -c "echo permit persist keepenv :wheel > /etc/doas.conf"
fi
