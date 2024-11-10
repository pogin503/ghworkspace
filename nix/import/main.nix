let
  add = import ./add.nix;
in
add {
  a = 1;
  b = 2;
}

# 省略して以下の書き方をすることが多い
# import ./add.nix { a = 1; b = 2; }
