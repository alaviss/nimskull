#!/usr/bin/env bash

# Script to package differences in the working tree compared to git index.
#
# This is used by CI to share workspace between parallel tasks.

set -e
set -u
set -o pipefail

nimSource=$(dirname "${BASH_SOURCE[0]}")/..

readarray -d '' diff < <(git -C "$nimSource" ls-files -zmo)
tar cf - "${diff[@]}" | zstdmt -o build/compiler.tar.zst
