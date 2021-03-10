-- スタック その2: 空状態、フル状態をサブタイプに
module stack2

-- スタックに積む値
sig Val {}

-- スタックの状態と操作
sig State {
 push: Val ->lone State,
 pop: lone State,
 top: lone Val
}

-- スタックが空の状態
one sig empty extends State {}

-- スタックがフルの状態
one sig full extends State {}

/* スタックに関する公理 */

-- スタックトップ値の取得
fact top {
 all s: State |
   (s != empty => one s.top)
   and
   (s  = empty => no s.top)
 }

-- ポップした後の状態
fact pop {
 all s: State |
   (s != empty => (one s.pop) and s.pop != s)
   and
   (s  = empty => no s.pop)
}

-- プッシュした後の状態
fact push {
 all s: State, v: Val |
   (s != full => one s.push[v] and s.push[v] != s)
   and
   (s  = full => no s.push[v])
}

-- プッシュした後のトップの値
fact push_top {
 all s: State, v: Val |
   s != full => s.push[v].top = v
} 

-- プッシュの後にポップすると
fact push_pop {
 all s: State, v: Val |
   s != full => (s.push[v]).pop = s
}

-- ポップを繰り返すと空になる
-- これがないと絵に不連結グラフが出てくる
fact pop_reachable {
  all x: State |
   x != full => x in full.^pop
}
