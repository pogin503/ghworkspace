-- モノイド
module monoid1 


-- 余代数形式の書き方
/* モノイドの指標 */
sig Mon {
 -- 乗法
 mult : Mon->one Mon
}

one sig pt {
 -- 単位
 unit : Mon
}

/* モノイドの公理 */

-- 結合律
fact assoc {
 all x, y, z : Mon | 
   (x.mult[y]).mult[z] = x.mult[y.mult[z]]
}

-- 単位律
fact unit {
 all x : Mon |
   pt.unit.mult[x] = x 
   and 
   x.mult[pt.unit] = x
}
