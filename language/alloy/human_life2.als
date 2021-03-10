
abstract sig Person {
  father:  lone Man,
  mother: lone Woman,
  sibling: set Person
}

sig Man extends Person {
  wife: lone Woman
}

sig Woman extends Person {
  husband: lone Man,
}

fun siblings (p: Person) : set Person {
}

pred sibling (p, p': Person) {
  sibling = p.mother = p'.mother and p.father = p'.father
}

fact {
  -- Biology
  no p: Person | p in p.^(mother + father + sibling)
  -- sibling = p: Person | 
  -- SocialConvention
  no (wife + husband ) & ^(mother + father)
}

fact Terminology {
  wife = ~husband
}

assert NoSelfFather {
  no m: Man | m = m.father
}
check NoSelfFather

fun grandpas (p: Person) : set Person {
  let parent = mother + father + father.wife + mother.husband |
    p.parent.parent & Man
}

pred ownGrandpa (p: Person) {
  p in grandpas [p]
}

pred show (p: Person) {
  p in grandpas [p]
  #p.sibling >= 2
}

run show for 6
