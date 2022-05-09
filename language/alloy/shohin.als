module exec/shohin

abstract sig Shohin{}

sig SimpleShohin extends Shohin{}

sig SetShohin extends Shohin{
  bundle: some SimpleShohin
}

fact {
  all s: Shohin | lone bundle.s
}

pred show{
}
run show
