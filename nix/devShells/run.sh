#!/usr/bin/env bash
set -x
git add .
nix develop --impure
