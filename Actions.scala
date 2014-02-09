import PlayerState.PlayerNumber

/* Actions are possible moves of a player. An action always wraps an option.
 * While options represent possible and impossible moves, sometimes including
 * decisions (like trade), all actions are legal moves which are playable
 * as-is, all required decisions already made.
 *
 * TODO "resource poker" (low prio)
 */
abstract class Action {
    // applies this action to one player of the game state
    // returns new player state, a possibly discarded card
    def apply(p: PlayerState, g: GameState): (PlayerState, Option[Card], List[(PlayerNumber, LateAction)])
    def describe(p: PlayerState, g: GameState): String
}

/* LateActions represent actions that happen each round after regular actions
 * are played, but before drafting. This includes passing trade money to other
 * players, and some special actions (ie, Halikarnassos and Babylon).
 * LateActions usually occur as tuples (PlayerNumber,LateAction)
 */
abstract class LateAction extends Action {
    override def apply(p: PlayerState, g: GameState): (PlayerState, Option[Card], List[(PlayerNumber, LateAction)])
        = apply(p, g, Nil)
    def apply(p: PlayerState, g: GameState, discardPile: List[Card]): (PlayerState, Option[Card], List[(PlayerNumber, LateAction)])
        = apply(p, g)
}

// LateApplicableActions are applicable as-is
abstract class LateApplicableAction extends LateAction

/* LateInteractiveActions require interaction from the user (usually yielding
 * an appropriate LateApplicableAction), which happens in the interface between
 * GameState.earlyDraft and GameState.lateDraft
 */
abstract class LateInteractiveAction extends LateAction

// pick a card to play. no resource checks are made here, since CardAvailable
// instances always represent cards which require no additional resources.
case class ActionPick(option: CardFree) extends Action {
    def apply(p: PlayerState, g: GameState) = p play(option.card, g)
    def describe(p: PlayerState, g: GameState) = s"Player ${p.name} builds $option"
}

// pick a card to play. trade decisions~
case class ActionPickWithTrade(option: CardTrade, trade: Trade) extends Action {
    def apply(p: PlayerState, g: GameState) = {
        (p.copy(gold = p.gold - trade.cost) play(option.card, g), None, List(
            if(trade.toLeft > 0) List( ((p.number-1+g.players.length) % g.players.length, LateTrade(trade.toLeft, p)) ) else Nil,
            if(trade.toRight > 0) List( ((p.number+1) % g.players.length, LateTrade(trade.toRight, p)) ) else Nil
        ).flatten)
    }
    def describe(p: PlayerState, g: GameState) = s"Player ${p.name} builds ${option}, trading " + (List(
        if(trade.toLeft > 0) s"${trade.toLeft} gold pieces to the left" else Nil,
        if(trade.toRight > 0) s"${trade.toRight} gold pieces to the right" else Nil
    ).filter( _ != Nil).mkString(" and "))
}

// pick a card to stuff as a wonder stage. no resource checks are made here,
// since WonderFree instances always represent stages that need no additional
// resources.
case class ActionWonder(option: WonderFree, card: CardOption) extends Action {
    def apply(p: PlayerState, g: GameState) = {
        val (state, lateops) = option.stage benefit p.copy(wonderStuffed = card.card :: p.wonderStuffed)
        (state, None, lateops)
    }
    def describe(p: PlayerState, g: GameState) = s"Player ${p.name} builds $option"
}
case class ActionWonderWithTrade(option: WonderTrade, trade: Trade, card: CardOption) extends Action {
    def apply(p: PlayerState, g: GameState) = {
        val (state, lateops) = option.stage benefit p.copy(wonderStuffed = card.card :: p.wonderStuffed)
        (state, None, List(
            if(trade.toLeft > 0) List( ((p.number-1+g.players.length) % g.players.length, LateTrade(trade.toLeft, p)) ) else Nil,
            if(trade.toRight > 0) List( ((p.number+1) % g.players.length, LateTrade(trade.toRight, p)) ) else Nil
        ).flatten ++ lateops)
    }
    def describe(p: PlayerState, g: GameState) = s"Player ${p.name} builds ${option}, trading " + (List(
        if(trade.toLeft > 0) s"${trade.toLeft} gold pieces to the left" else Nil,
        if(trade.toRight > 0) s"${trade.toRight} gold pieces to the right" else Nil
    ).filter( _ != Nil).mkString(" and "))
}

// Discard some card
case class ActionDiscard(option: CardOption) extends Action {
    def apply(p: PlayerState, g: GameState) = (p addGold(3) discard(option.card), Some(option.card), Nil)
    def describe(p: PlayerState, g: GameState) = s"Player ${p.name} discards a card for 3 gold pieces"
}

case class LateTrade(amount: Int, from: PlayerState) extends LateApplicableAction {
    override def apply(p: PlayerState, g: GameState) = p addGold(amount)
    def describe(p: PlayerState, g: GameState) = s"Player ${p.name} gets $amount gold pieces from ${from.name}"
}

/* A trade is a trade offer made by the interface. It can be checked for
 * validity, returning the associated Action. This should be the only way
 * for the UI to ever acquire Actions involving trade.
 */
case class Trade(toLeft: Int, toRight: Int) {
    lazy val cost = toLeft + toRight

    def tradeOffer(p: PlayerState, t: CardTrade): Option[ActionPickWithTrade] = {
        if(t.costsPossible(p, toLeft, toRight))
            Some(ActionPickWithTrade(t, this))
        else
            None
    }
    def tradeOffer(p: PlayerState, t: WonderTrade, card: CardOption): Option[ActionWonderWithTrade] = {
        if(t.costsPossible(p, toLeft, toRight))
            Some(ActionWonderWithTrade(t, this, card))
        else
            None
    }

}
