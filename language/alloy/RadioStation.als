
sig Singer {
  band: Band
} {
}

sig RadioStation {
  band: set Freq
}

fact NoOverlapping {
  /* ラジオ局同士の(周波数)バンドが重ならない */
  no disj s, s' : RadioStation | some s.band & s'.band
}

fact {
  /* (歌の)バンドには少なくとも一人の歌手がいること */
  all b: Band | some b.~band
  /*
    all b: Band | some b.~band
    all b: Band | some band.b
    all b: Band | some s: Singer | s -> b in Band
  */
}
sig Band, Freq {}
