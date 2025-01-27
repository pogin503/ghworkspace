#!/usr/bin/env bash
set -x
git add .
nix flake show
# nix run .#hello
