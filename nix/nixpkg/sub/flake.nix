{
  # add関数をエクスポートするflake
  outputs = _inputs: {
    add_a_b = a: b: a + b;
  };
}
