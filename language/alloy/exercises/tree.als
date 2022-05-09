module exercises/tree
sig Tree {
  root: Node
  right: Tree -> Tree
  left: Tree -> Tree
}


pred isTree (r: unitv -> univ) {
/* hasNode, hasLeaf, left, right
    木の性質とは
 */
}
run isTree for 4
