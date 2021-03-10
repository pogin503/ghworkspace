-- 自然数の区間 {0, 1, 2, ..., n}
module interval

sig Interv {
 -- 後者
 next: lone Interv,
 -- 距離
 private delta0: Interv ->one Interv,
 delta: Interv ->one Interv
}

-- ゼロと最後の数
one sig zero, last extends Interv {}

-- nextに関する公理
fact next {
 -- lastのnextは存在しない
 no last.next

and
 -- last以外ではnextが一意に定義されている
 all x: (Interv - last) |
   one x.next

and
 -- ゼロからの可達性
 zero.*next = Interv

}

-- delta0に関する公理
fact delta0 {
 all x, y: Interv |
 {
   x.delta0[x] = zero
   -- x ≦ y ならば、定義されている範囲内で
   -- δ0(x, y.next) = δ0(x, y).next
   y in x.*next implies
     (x.delta0[y.next] in (x.delta[y]).next)
 }
}

-- deltaに関する公理
fact delta {
 all x, y: Interv |
   -- x ≦ y ならば、δ = δ0
   y in x.*next implies (delta = delta0)
   -- それ以外では、δ(x, y) = δ(y, x)
   else x.delta[y] = y.delta0[x]
}
