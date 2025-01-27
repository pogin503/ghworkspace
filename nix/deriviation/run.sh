#!/usr/bin/env bash
set -x
script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" &>/dev/null && pwd -P)
pushd "$script_dir"
# git add .
nix eval --file ./drv.nix
cat /nix/store/ybkx07yfg2w33mr909bk2g7z0264sy4x-hello-txt.drv
# sleep 1
nix-store --realise /nix/store/ybkx07yfg2w33mr909bk2g7z0264sy4x-hello-txt.drv
cat /nix/store/z4j03hs3qk7a3cbiwglgys2cz61pbi6s-hello-txt
nix derivation show --file ./drv.nix
# {
#   "/nix/store/ybkx07yfg2w33mr909bk2g7z0264sy4x-hello-txt.drv": {
#     "args": [
#       "-c",
#       "echo -n Hello > $out"
#     ],
#     # ビルドを実行する実行可能ファイルのパス
#     "builder": "/bin/sh",
#     "env": {
#       "builder": "/bin/sh",
#       "name": "hello-txt",
#       "out": "/nix/store/z4j03hs3qk7a3cbiwglgys2cz61pbi6s-hello-txt",
#       "system": "x86_64-linux"
#     },
#     "inputDrvs": {},
#     "inputSrcs": [],
#     # パッケージ名
#     "name": "hello-txt",
#     "outputs": {
#       # ビルド成果物を配置するストアパス
#       "out": {
#         "path": "/nix/store/z4j03hs3qk7a3cbiwglgys2cz61pbi6s-hello-txt"
#       }
#     },
#     # ビルドターゲットのプラットフォーム
#     "system": "x86_64-linux"
#   }
# }

nix store delete /nix/store/z4j03hs3qk7a3cbiwglgys2cz61pbi6s-hello-txt
#  Import From Derivation
nix eval --file ./IFD.nix
