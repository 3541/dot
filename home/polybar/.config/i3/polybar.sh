#!/bin/sh

set -e

pkill polybar || true

while pgrep -x polybar > /dev/null; do sleep 1; done

for m in $(xrandr -q | grep ' connected' | cut -d' ' -f1); do
    MONITOR="$m" polybar --config="$HOME/.config/polybar/config.ini" top > "/tmp/bar-$m.log" 2>&1 &
done

