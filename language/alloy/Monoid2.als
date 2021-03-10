-- モノイド その2: 単位も乗法もグローバルに定義してみる
module monoid2 


/* モノイドの指標 */

-- 単なる集合
sig Mon {}

-- 単位と乗法はグローバルに
one sig pt {
 -- 単位
 unit : Mon,
 -- 乗法
 mult : Mon-> (Mon-> one Mon)
}

/*モノイドの公理 */

-- 結合律
fact assoc {
 all x, y, z : Mon |
   pt.mult[pt.mult[x, y], z] = pt.mult[x, pt.mult[y, z]]
}

-- 単位律
fact unit {
 all x : Mon |
   pt.mult[pt.unit, x] = x 
   and 
   pt.mult[x, pt.unit] = x
}
