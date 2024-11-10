#!/usr/bin/env bash
set -x
git add .
nix eval .#hello
nix eval .#sum_1_2
nix eval .#value
