module ProgrammingLanguage

open util/boolean

sig Language {
  -- name: String,
  version: set Version,
  examples: version -> set Example
} {
}
 
sig Version {}

sig Example {
  runnable: Runnable,
  categories: some Category
}

abstract sig Runnable extends Bool {}
one sig NotYetChecking, IsError, IsRunnbale extends Runnable {}

sig Category {
  -- name: String,
  -- displayable: boolean,
  height: Int,
  parent: Category
} {
  height >= 0
}

pred isRoot {
  -- all c: Category | c in c.height[0]
  -- {c: Category | c.height = 0}
}

pred show {
}

run show for 3 but 3 Language
