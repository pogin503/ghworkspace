module exercises/properties

run {
  some r: univ -> univ {
    some r             -- 非空的
    r.r in r              -- 推移的
    -- no iden & r       -- 非反射的
    ~r in r              -- 対称的
    ~r.r in iden       -- 関数的
    r.~r in iden       -- 単射的
    univ in r.univ    -- 全域
    univ in univ.r    -- 全射的
  }
} for 4
