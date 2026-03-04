# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a Nix language learning repository containing examples and experiments covering various Nix concepts, from basic expressions to flakes and advanced features. The repository is organized by topic, with each subdirectory containing self-contained examples.

## Directory Structure

Each topic-specific directory contains:
- `.nix` files - Nix expressions demonstrating specific concepts
- `flake.nix` - Flake configuration (for flake-based examples)
- `run.sh` - Executable script that demonstrates how to use the examples

Key directories:
- `AttrSet/` - Attribute set operations and special attributes (e.g., `__toString`)
- `deriviation/` - Derivation basics and Import From Derivation (IFD)
- `devShells/` - Development shell configurations using flakes
- `hello-world_flake/` - Basic flake with outputs and sub-flake inputs
- `home-manager_sample/` - Home Manager configuration examples
- `import/` - Module import patterns and function composition
- `impure/` - Impure evaluation examples
- `nix-cli/` - Nix CLI commands (build, run, shell, develop, fmt)
- `nix_template/` - Comprehensive flake template showing all standard outputs
- `nixpkg/` - Package management examples
- `overlays/` - Overlay patterns for modifying packages

## Common Commands

### Running Examples

Each directory has a `run.sh` script that demonstrates the example:
```bash
cd <directory>
./run.sh
```

### Basic Nix Operations

Evaluate a Nix expression:
```bash
nix eval --file <filename>.nix
```

Evaluate a flake output:
```bash
nix eval .#<output-name>
```

### Flake Commands

Development shell (enter a development environment):
```bash
nix develop .#<shell-name>        # Named shell
nix develop                       # Default shell
nix develop --impure              # With impure evaluation
```

Build a package:
```bash
nix build .#<package-name>
nix build nixpkgs#<package>       # From nixpkgs
```

Run a package:
```bash
nix run .#<app-name>
nix run nixpkgs#<package>         # From nixpkgs
```

Temporary shell with packages:
```bash
nix shell nixpkgs#<package>
```

### Derivation Commands

Show derivation details:
```bash
nix derivation show --file <filename>.nix
```

Realize a derivation:
```bash
nix-store --realise <derivation-path>
```

## Architecture Patterns

### Example Structure

The repository follows a consistent pattern:
1. Each topic has a dedicated directory with focused examples
2. Examples use `run.sh` scripts that include `git add .` before evaluation (required for flakes to track files)
3. Scripts use `set -x` for visibility and proper directory handling with `script_dir`

### Flake Patterns

Most flakes follow the structure:
```nix
{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        # packages, devShells, apps, etc.
      }
    );
}
```

### Run Script Pattern

Standard `run.sh` structure:
```bash
#!/usr/bin/env bash
set -x
script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" &>/dev/null && pwd -P)
pushd "$script_dir"
git add .  # Required for flakes to see new/modified files
# ... example commands ...
```

## Important Notes

### Git Staging Requirement

Flakes only track files that are staged in git. The `run.sh` scripts include `git add .` to ensure all files are visible to Nix flake commands.

### Nix Profile Usage

When using nix commands, prefer `nix profile` for package management operations (as per user's global instructions).

### Impure Evaluation

Some examples require `--impure` flag for evaluation when they access external state or use impure features.
