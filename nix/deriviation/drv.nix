derivation {
  name = "hello-txt";
  builder = "/bin/sh";
  args = [
    "-c"
    "echo -n Hello > $out"
  ];
  system = builtins.currentSystem;
}
