#!/usr/bin/env bash
set -x
script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" &>/dev/null && pwd -P)
pushd "$script_dir"
git add .
cat ./flake.nix
nix flake show
# nix run .#hello
