#!/bin/sh

set -e

if [ "$(uname)" = "Darwin" ]; then
    # macOS does not support readlink -f.
    cd $(perl -MCwd=abs_path -le 'print abs_path readlink(shift);' "$0")
else
    cd $(dirname $(readlink -f "$0"))
fi

export DOT_ROOT="$(pwd)"
export SCRIPTS="$DOT_ROOT/scripts-new"

"$SCRIPTS/install-lix.sh"
"$SCRIPTS/install-config-orchestrator.sh"
