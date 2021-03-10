-- モノイド その3: 単位にサブタイプを使う
module monoid3


/* モノイドの指標 */

sig Mon {
 -- 乗法
 mult : Mon->one Mon
}

-- 単位元（単元の部分集合）
one sig unit extends Mon {}

/* モノイドの公理 */

-- 結合律
fact assoc {
 all x, y, z : Mon | 
   (x.mult[y]).mult[z] = x.mult[y.mult[z]]
}

-- 単位律
fact unit {
 all x : Mon |
   unit.mult[x] = x 
   and 
   x.mult[unit] = x
}
