-- 人生
module human_life

-- 人
abstract sig person {
 mother: lone woman,
 private sibling: set person
}

-- 男
sig man extends person {
 wife: lone woman
}

-- 女
sig woman extends person {
 private husband: lone man,
 private child: set person
}

-- 母親は自分ではない
fact mother {
 no (^mother & iden)
}

-- 一夫一婦
fact married_couple {
 husband = ~wife
}

-- 子供と兄弟の定義
fact child_sibling {
 {
  child = ~mother

  sibling = mother.child
 }
}

-- 近親結婚の禁止
fact forbit_intermarriage {
 no (wife & ^mother)
 no (wife & sibling)
}

pred show {}

/* コマンド */
run show
run {#person =5} for 5
