import PlayerState.PlayerNumber

abstract class Action {
    // applies this action to one player of the game state
    // returns new player state, a possibly discarded card
    def apply(p: PlayerState, g: GameState): (PlayerState, Option[Card], List[(PlayerNumber, Action)])
}

// case class BuildWonder

// pick a card and play it. may involve trading!
// TODO make sure all prerequisites are fulfilled
case class CardPick(card: Card) extends Action {
    def apply(p: PlayerState, g: GameState) = (p play card, None, Nil)
}

// discard a card
case class CardDiscard(card: Card) extends Action {
    def apply(p: PlayerState, g: GameState) = (p.copy(gold = p.gold + 3), Some(card), Nil)
}

case class TradeMoney(amount: Int) extends Action {
    def apply(p: PlayerState, g: GameState) = (p.copy(gold = p.gold + amount), None, Nil)
}
