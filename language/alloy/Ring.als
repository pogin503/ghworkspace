sig Node {
  next: Node, 
  value: Value one -> Time
}

sig Time { next: Time }
sig Value {}

/** ノードがリング状になっている */
fact  { all n: Node | Node in n.^next }

/** 循環がない */
fact { no ^(Time <: next) & iden }
