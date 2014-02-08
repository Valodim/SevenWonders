import PlayerState.PlayerNumber

/* Actions are possible moves of a player. An action always wraps an option.
 * While options represent possible and impossible moves, sometimes including
 * decisions (like trade), all actions are legal moves which are playable
 * as-is, all required decisions already made.
 *
 * As of now, there MAY be exceptions to this where insufficient funds lead
 * to an implicit discard action. This is subject to changes.
 *
 * TODO move trade logic slightly upstream
 * TODO "resource poker" (low prio)
 */

abstract class Action {
    // applies this action to one player of the game state
    // returns new player state, a possibly discarded card
    def apply(p: PlayerState, g: GameState): (PlayerState, Option[Card], List[(PlayerNumber, Action)])
}

// pick a card to play. no resource checks are made here, since CardAvailable
// instances always represent cards which require no additional resources.
case class ActionPick(option: CardAvailable) extends Action {
    def apply(p: PlayerState, g: GameState) = (p play(option.card, g), None, Nil)
}
// pick a card to play. trade decisions~
case class ActionPickWithTrade(option: CardTrade, trade: TradeCard) extends Action {
    def apply(p: PlayerState, g: GameState) = {
        (p.copy(gold = p.gold - trade.cost) play(option.card, g), None, List(
            ( (p.number-1+g.players.length) % g.players.length, TradeMoney(trade.toLeft)),
            ( (p.number+1) % g.players.length, TradeMoney(trade.toRight))
        ))
    }
}

// pick a card to stuff as a wonder stage. no resource checks are made here,
// since WonderFree instances always represent stages that need no additional
// resources.
case class ActionWonder(option: WonderFree, card: CardOption) extends Action {
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
