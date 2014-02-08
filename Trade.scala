abstract class Trade {
    val cost: Int
    val toLeft: Int
    val toRight: Int
}

case class TradeCard(card: CardTrade, cost: Int, toLeft: Int, toRight: Int) extends Trade
case class TradeWonder(stage: WonderTrade, cost: Int, toLeft: Int, toRight: Int) extends Trade

object Trade {

    /*
    // make sure it's enough?
    if(! (option.card.resourceReq - p.allResources - left - right).isEmpty) {
        println("error, resource requirements not fulfilled. discarding.")
        ActionDiscard(option)(p, g)
    // OH SNAP
    } else if(costLeft + costRight + option.card.goldCost > p.gold) {
        println(s"error, not enough gold ($costLeft+$costRight+${option.card.goldCost}) - discarding.")
        ActionDiscard(option)(p, g)
    } else {

    // accumulate cost
    val costLeft = (left grouped(4)) zip Iterator(p.tradeLeft._1, p.tradeLeft._2) flatMap { case (x,y) => x map (_*y) } reduce (_ + _)
    val costRight = (right grouped(4)) zip Iterator(p.tradeRight._1, p.tradeRight._2) flatMap { case (x,y) => x map (_*y) } reduce (_ + _)
    */

}
