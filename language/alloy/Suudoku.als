module suudoku

abstract sig Number { data: Number -> Number }
-- Region1 3x3
-- 
abstract sig Region1, Region2, Region3 extends Number {}

one sig N1, N2, N3 extends Region1 {}
one sig N4, N5, N6 extends Region2 {}
one sig N7, N8, N9 extends Region3 {}

pred complete(rows: set Number, cols: set Number) {
  -- 指定された列、行にN1〜N9が1つずつ存在
  Number in cols.(rows.data)
}

pred rules() {
  -- N1〜N9は1つずつ、かつN1〜N9のマップを保持している
  all x, y: Number { lone y.(x.data) }
  
  all row: Number { complete[row, Number] }
  all col: Number { complete[Number, col] }
  complete[Region1, Region1]
  complete[Region1, Region2]
  complete[Region1, Region3]
  complete[Region2, Region1]
  complete[Region2, Region2]
  complete[Region2, Region3]
  complete[Region3, Region1]
  complete[Region3, Region2]
  complete[Region3, Region3]
}
