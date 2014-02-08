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
    def apply(p: PlayerState, g: GameState): (PlayerState, Option[Card], List[(PlayerNumber, LateAction)])
    def describe(p: PlayerState, g: GameState): String
}
abstract class LateAction extends Action {
    def apply(p: PlayerState, g: GameState, discardPile: List[Card]): (PlayerState, Option[Card], List[(PlayerNumber, LateAction)])
        = apply(p, g)
}

// pick a card to play. no resource checks are made here, since CardAvailable
// instances always represent cards which require no additional resources.
case class ActionPick(option: CardAvailable) extends Action {
    def apply(p: PlayerState, g: GameState) = (p play(option.card, g), None, Nil)
    def describe(p: PlayerState, g: GameState) = s"Player ${p.name} builds $option"
}
// pick a card to play. trade decisions~
case class ActionPickWithTrade(option: CardTrade, trade: Trade) extends Action {
    def apply(p: PlayerState, g: GameState) = {
        (p.copy(gold = p.gold - trade.cost) play(option.card, g), None, List(
            ( (p.number-1+g.players.length) % g.players.length, TradeMoney(trade.toLeft, p)),
            ( (p.number+1) % g.players.length, TradeMoney(trade.toRight, p))
        ))
    }
    def describe(p: PlayerState, g: GameState) = s"Player ${p.name} builds ${option}, trading ${trade.toLeft} gold pieces to the left and ${trade.toRight} to the right"
}

// pick a card to stuff as a wonder stage. no resource checks are made here,
// since WonderFree instances always represent stages that need no additional
// resources.
case class ActionWonder(option: WonderFree, card: CardOption) extends Action {
    def apply(p: PlayerState, g: GameState) = {
        // todo, resource requirements and boundary checks
        (option.stage benefit p.copy(wonderStuffed = card.card :: p.wonderStuffed), None, Nil)
    }
    def describe(p: PlayerState, g: GameState) = s"Player ${p.name} builds $option"
}
case class ActionWonderWithTrade(option: WonderTrade, trade: Trade, card: CardOption) extends Action {
    def apply(p: PlayerState, g: GameState) = {
        (option.stage benefit p.copy(wonderStuffed = card.card :: p.wonderStuffed), None, List(
            ( (p.number-1+g.players.length) % g.players.length, TradeMoney(trade.toLeft, p)),
            ( (p.number+1) % g.players.length, TradeMoney(trade.toRight, p))
        ))
    }
    def describe(p: PlayerState, g: GameState) = s"Player ${p.name} builds ${option}, trading " + List(
        if(trade.toLeft > 0) "${trade.toLeft} gold pieces to the left" else Nil,
        if(trade.toRight > 0) "${trade.toRight} gold pieces to the right" else Nil
    ).mkString(" and ")
}

// discard a card
case class ActionDiscard(option: CardOption) extends Action {
    def apply(p: PlayerState, g: GameState) = (p addGold(3) discard(option.card), Some(option.card), Nil)
    def describe(p: PlayerState, g: GameState) = s"Player ${p.name} discards a card for 3 gold pieces"
}

case class TradeMoney(amount: Int, from: PlayerState) extends LateAction {
    def apply(p: PlayerState, g: GameState) = (p addGold(amount), None, Nil)
    def describe(p: PlayerState, g: GameState) = s"Player ${p.name} gets $amount gold pieces from Player ${from.name}"
}
