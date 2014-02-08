import PlayerState.PlayerNumber

abstract class Action {
    // applies this action to one player of the game state
    // returns new player state, a possibly discarded card
    def apply(p: PlayerState, g: GameState): (PlayerState, Option[Card], List[(PlayerNumber, Action)])
}

// pick a card and play it. may involve trading!
case class ActionPick(option: CardAvailable) extends Action {
    def apply(p: PlayerState, g: GameState) = (p play(option.card, g), None, Nil)
}
case class ActionPickWithTrade(option: CardTrade, left: Resources, right: Resources) extends Action {
    def apply(p: PlayerState, g: GameState) = {

        // accumulate cost
        val costLeft = (left grouped(4)) zip Iterator(p.tradeLeft._1, p.tradeLeft._2) flatMap { case (x,y) => x map (_*y) } reduce (_ + _)
        val costRight = (right grouped(4)) zip Iterator(p.tradeRight._1, p.tradeRight._2) flatMap { case (x,y) => x map (_*y) } reduce (_ + _)

        // make sure it's enough?
        if(! (option.card.resourceReq - p.allResources - left - right).isEmpty) {
            println("error, resource requirements not fulfilled. discarding.")
            ActionDiscard(option)(p, g)
        // OH SNAP
        } else if(costLeft + costRight + option.card.goldCost > p.gold) {
            println(s"error, not enough gold ($costLeft+$costRight+${option.card.goldCost}) - discarding.")
            ActionDiscard(option)(p, g)
        } else {
            (p.copy(gold = p.gold - costLeft - costRight) play(option.card, g), None, List(
                ( (p.number-1+g.players.length) % g.players.length, TradeMoney(costLeft)),
                ( (p.number+1) % g.players.length, TradeMoney(costRight))
            ))
        }
    }
}

case class ActionWonder(option: WonderOption, card: CardOption) extends Action {
    def apply(p: PlayerState, g: GameState) = {
        val stage = p.wonder.stages(p.wonderStuffed.length)
        // todo, resource requirements and boundary checks
        (stage benefit p.copy(wonderStuffed = card.card :: p.wonderStuffed), None, Nil)
    }
}

// discard a card
case class ActionDiscard(option: CardOption) extends Action {
    def apply(p: PlayerState, g: GameState) = (p addGold(3) discard(option.card), Some(option.card), Nil)
}

case class TradeMoney(amount: Int) extends Action {
    def apply(p: PlayerState, g: GameState) = (p addGold(amount), None, Nil)
}
