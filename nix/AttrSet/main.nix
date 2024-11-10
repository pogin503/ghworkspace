let
  attr = {
    someAttr = 1234;

    # toString適用時、selfにはattr自体が渡される
    __toString = self: self.someAttr;
  };
in
builtins.toString attr

# 評価結果: "1234"
