#!/usr/bin/env bash
set -euo pipefail

mkdir_and_cp() {
    local src="$1"
    local dst="$2"

    if [ -e "$src" ]; then
        mkdir -p "$(dirname "$dst")"
        cp -r "$src" "$dst"
    else
        echo "Warning: '$src' does not exist, skipping."
    fi
}

for doc in dist-newstyle/build/*/*/*/doc/html/helma/; do
    mkdir_and_cp "$doc" docs/reports
done

for hpc in dist-newstyle/build/*/*/*/hpc/vanilla/html/helma-test/; do
    mkdir_and_cp "$hpc" docs/reports
done