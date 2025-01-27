#!/usr/bin/env bash
set -x
script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" &>/dev/null && pwd -P)
pushd "$script_dir"
nix eval --file ./main.nix
