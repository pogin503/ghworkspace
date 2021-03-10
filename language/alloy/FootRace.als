sig FootRace {
    runner : seq Person
} {
  Person in Int.runner
  all p : Person | one p.~runner
}

abstract sig Person {}
one sig Ichiro, Jiro, Saburo extends Person {
}

pred word1() {
  Ichiro.rank != 0
}

pred word2() {
  Jiro.rank = 0
}

pred word3() {
  Saburo.rank = 1
}

fun rank: Person -> Int {
  ~(FootRace.runner)
}

pred ichiroIsLiar {
  not word1
  word2
  word3
}

pred jiroIsLiar {
  word1
  not word2
  word3
}

pred saburoIsLiar {
  word1
  word2
  not word3
}

pred show() {}

-- run show
run ichiroIsLiar
run jiroIsLiar
run saburoIsLiar


