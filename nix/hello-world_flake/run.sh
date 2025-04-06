#!/usr/bin/env bash
set -x
script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" &>/dev/null && pwd -P)
pushd "$script_dir"
git add .
cat flake.nix
nix eval .#hello
nix eval .#sum_1_2
nix eval .#value
