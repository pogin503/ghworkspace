{
  # inputs = { };

  outputs = _: {
    now = builtins.currentTime;
    user = builtins.getEnv "USER";
    editor = builtins.getEnv "EDITOR";
    system = builtins.currentSystem;
  };
}
