#!/usr/bin/env bash
set -x
git add .

DESC=$(cat << EOS

# ビルド
nix build <flake-url>#<パッケージ名>

# パッケージ
packages.<プラットフォーム>.<指定したパッケージ名> = <Derivation>;

# パッケージ（Nixpkgs）
legacyPackages.<プラットフォーム>.<パッケージ名> = <Derivation>;
EOS
)

nix build nixpkgs#hello
readlink result
ls result

DESC=$(cat << EOS

# 実行
nix run <flake-url>#<パッケージ名>

# パッケージ
packages.<プラットフォーム>.<指定したパッケージ名> = <Derivation>;

# パッケージ（Nixpkgs）
legacyPackages.<プラットフォーム>.<パッケージ名> = <Derivation>;

# apps
apps."<プラットフォーム>"."<app名>" = {
  type = "app";
  program = "<ストアパス>";
};
EOS
)
nix shell nixpkgs#hello
hello
exit
hello


DESC=$(cat << EOS

devShells.${system}.${name} = <Derivation>;

nix develop <flake-url>#<name>

nix fmt <flake-url>

formatter.<プラットフォーム> = <Derivation>;
EOS
)
