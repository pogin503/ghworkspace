module exercises/spanning
pred isTree(r: univ -> univ) {
}
pred spans (r1, r2: univ -> univ) {
}
pred show (r, t1, t2: univ -> univ) {
  spans[t1, r] and isTree [t1]
  spans[t2, r] and isTree [t2]
  t1 not = t2
}

run show for 3

