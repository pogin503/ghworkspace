#!/usr/bin/env bash
set -x
nix eval .#now
nix eval --impure .#now
nix eval .#user
nix eval --impure .#user
nix eval --impure .#system
nix eval --impure .#editor
