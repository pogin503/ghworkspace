import Lake
open Lake DSL

require mathlib from git
  "https://github.com/leanprover-community/mathlib4"

package myleanproj {
  -- add package configuration options here
}

lean_lib Myleanproj {
  -- add library configuration options here
}

@[default_target]
lean_exe test {
   root := `Main
}
