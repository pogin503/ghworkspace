module exercises/clojure

pred transCover (R, r: univ -> univ) {

}

pred transClosure (R, r: univ -> univ) {
  transCover [R, r]
  
}

assert Equivalence {
  all R, r: univ -> univ | transClosure [R, r] iff R = ^r
}

check Equivalence for 3

