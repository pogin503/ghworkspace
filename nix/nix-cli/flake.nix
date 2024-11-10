{
  description = "hello";
  
  inputs = {
    sub_flake.url = "path:./sub";
  };

  outputs = { sub_flake, ... }: {
  # outputs = { self, inputs }: {
    hello = "Hello, world!";
    sum_1_2 = sub_flake.add_a_b 1 2;
    value = 1;
  };

}
