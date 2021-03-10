-- 自然数の区間 {0, 1, 2, ..., n}
module interval

sig Interv {
 -- 後者
 next: lone Interv,
 -- 順序 ＜
 lt: set Interv
}

-- ゼロと最後の数
one sig zero, last extends Interv {}

-- nextに関する公理
fact next {
 -- lastのnextは存在しない
 no last.next

and
 -- last以外ではnextが定義されている
 all x: (Interv - last) |
   one x.next

and
 -- ゼロからの可達性
 zero.*next = Interv

}

-- ltに関する公理
fact lt {
 lt = ^next
}
