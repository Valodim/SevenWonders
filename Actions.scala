abstract class Action {
    def apply(p: PlayerState): PlayerState
}

// case class BuildWonder

// pick a card and play it. may involve trading!
// TODO make sure all prerequisites are fulfilled
case class CardPick(card: Card) extends Action {
    def apply(p: PlayerState): PlayerState = p play card
}

// discard a card
case class CardDiscard(card: Card) extends Action {
    def apply(p: PlayerState): PlayerState = p.copy(gold = p.gold + 3)
}

