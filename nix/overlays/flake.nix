{
  # Flakeの入力セクション。必要なFlakeのソースを定義する。
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    # flake-utilsを使用して共通タスクを簡単に処理できるようにする。
    flake-utils.url = "github:numtide/flake-utils";
    # Nix User Repository (NUR) を使用してユーザ定義パッケージを利用可能にする。
    nur.url = "github:nix-community/NUR";
  };

  # Flakeの出力セクション。出力の値がFlakeを利用する他の場所で使われる。
  outputs =
    {
      nixpkgs,
      flake-utils,
      nur,
      ...
    }:
    # flake-utilsの機能を使い、サポートされている全てのシステム
    #（例: x86_64-linux, aarch64-darwin）に対して構成を定義します。
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        # NURのオーバーレイを設定。これによりNURのパッケージが利用可能になる。
        overlays = [ nur.overlay ];
        # Nixpkgsを指定したオーバーレイと共にインポートし、パッケージセットを生成する。
        pkgs = import nixpkgs { inherit system overlays; };
      in
      {
        # Flakeが出力するパッケージを定義する。
        packages = {
          default = pkgs.nur.repos.mic92.hello-nur;
        };
      }
    );

}
