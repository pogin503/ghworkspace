#!/usr/bin/env bash
set -x
script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" &>/dev/null && pwd -P)
pushd "$script_dir"
nix eval .#now
nix eval --impure .#now
nix eval .#user
nix eval --impure .#user
nix eval --impure .#system
nix eval --impure .#editor
