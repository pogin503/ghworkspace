module consumption_tax_rate

open util/ordering [Day]

/*
軽減税率で8%→9%->8%のようなモデルを作りたい
Item, 
2014/04/01〜2019/09/30 8%
2019/09/30〜9999/12/31 10%, 8%
通常12%, 軽減8%, 
通常10%, 軽減8%の期間
通常12%, 軽減8%
*/ 

sig Day {}

abstract sig Float {
//    leftPart: Int,
//    rightPart: Int
} 
sig FivePercent, EightPercent, NinePercent, TenPercent, TwelvePercent extends Float {}


abstract sig 消費税 {
  startDate: some Day,
  endDate: some Day,
  rate: Float
} {
  //  rate.leftPart <= 1
  //  rate.rightPart > 0
  // 開始日と終了日は交わらない 
  lt[startDate, endDate]
}

sig 通常税率 extends 消費税 {}

sig 軽減税率 extends 消費税 {}

fun outsideOfPeriodTax(c: 消費税, d: Day) : set 消費税 {
   ! (lt[d, c.startDate] or lt[c.endDate, d]) implies c else none
}

fun getComsumptionTax(c: 消費税, d: Day): set 消費税 {
  // 開始日 <= d and d <= 終了日
  (lte[c.startDate, d] and lte[d, c.endDate]) implies c else none
}

/** (間違い)日付を指定したら高々1個しか税率は取れないはず。 */
assert WrongAssert_existsComsumptionTax {
  lone getComsumptionTax[軽減税率, Day]
}
// check WrongAssert_existsComsumptionTax

assert OneReducedComsumptionTaxRate {
  lone itm: Item, ra: ReducedAccountingItem, c: 軽減税率, d: Day |
    ra -> getComsumptionTax[c, d] in itm.consumptionTaxRate
}

check OneReducedComsumptionTaxRate
/*
assert existsOutsideOfPeriodTax {
  // 反例が存在する。必ず
  some outsideOfPeriodTax[消費税, Day]
}
check existsOutsideOfPeriodTax
*/

abstract sig AccountingItem {}
sig NormalAccountingItem extends AccountingItem {}

sig ReducedAccountingItem extends AccountingItem {}

fact AccountingItemAnd {
  // 特定の勘定科目が必ず軽減税率、通常税率となるモデル
  // 軽減税率と通常税率の勘定科目が交わることはない
  no item : Item, na: NormalAccountingItem, tax: 軽減税率 |  
    na -> tax in item.consumptionTaxRate
  // 通常税率と軽減税率の勘定科目が交わることはない
  no item : Item, ra: ReducedAccountingItem, tax: 通常税率 |  
    ra -> tax in item.consumptionTaxRate

}

abstract sig Item {
  accountingItem: AccountingItem,
  consumptionTaxRate: accountingItem -> 消費税,  
  private prePrice: Int,
  price: prePrice ->some consumptionTaxRate,
} {
  prePrice >= 0
  // all d: Day | 
  //  lte[消費税.startDate, d] and lte[d, 消費税.endDate]

}

pred show[] {
  #通常税率.rate > 1
  #軽減税率.rate > 1
  // #ReducedAccountingItem > 1
  // #NormalAccountingItem > 1
  #Item > 4
//  Float.leftPart = 0
//  Float.rightPart > 0 
}
-- run show for 5 but 3 消費税, 2 通常消費税, 3 軽減税率
run show for 5 but 3 通常税率, 3 軽減税率
run show for 10




