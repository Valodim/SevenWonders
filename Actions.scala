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
case class ActionDiscard(card: Card) extends Action {
    def apply(p: PlayerState, g: GameState) = (p addGold(3) discard(card), Some(card), Nil)
    def describe(p: PlayerState, g: GameState) = s"Player ${p.name} discards a card for 3 gold pieces"
}
object ActionDiscard {
    def apply(option: CardOption): ActionDiscard = ActionDiscard(option.card)
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

/* A PlayerOption represents any kind of gameplay option a player may have,
 * like cards that can be played. The specific subclass contains information
 * about the type of option (card, wonder stage, ...) and in particular if it
 * is a legal move or not.
 *
 * Legal instances of PlayerOption may be wrapped into an appropriate Action,
 * which can then be applied to a PlayerState, usually as the turn of a player.
 */
abstract class PlayerOption

/* The TradeOption trait adds required resources that must be acquired by
 * trading. An instance including this trait assumes that the specified
 * resources are actually available, since other cases should lead to
 * OptionUnavailable.
 *
 * The main purpose of this trait is managing different possible ways of
 * trading for resources, especially those which may be acquired from either
 * neighbor and possibly at different costs.
 */
trait TradeOption {
    val either: Resources
    val left: Resources
    val right: Resources

    lazy val eitherSplit = either.split

    def fixedSums(p: PlayerState): (Int, Int) = (
            (left zip p.costsLeft).map{ case (x,y) => x*y }.sum,
            (right zip p.costsRight).map{ case (x,y) => x*y }.sum
        )

    /* returns a list of pairs of costs to purchase each singleton of the
     * "either" resources from the left and right neighbor
     */
    def splitCosts(p: PlayerState): List[(Int, Int)] =
        eitherSplit.map(x => (x zip p.costsLeft).collect{ case(1,y) => y }.sum) zip
        eitherSplit.map(x => (x zip p.costsRight).collect{ case(1,y) => y }.sum)

    // returns true if the given costs are feasible in any combination of spending
    def costsPossible(p: PlayerState, toLeft: Int, toRight: Int): Boolean = {
        allCosts(p) exists( _ == (toLeft, toRight) )
    }

    // returns the minimum cost for this trade
    def minCosts(p: PlayerState): Int = {
        allCosts(p) map { case (l,r) => l+r } min
    }

    // returns all possible combinations of left and right costs for this trade
    def allCosts(p: PlayerState): List[(Int,Int)] = {

        // accumulate cost for left-only and right-only resources
        val (costLeft, costRight) = fixedSums(p)

        // nothing left? early true, then
        if(either.isEmpty)
            return List((costLeft, costRight))

        // check if what's left of our funds works for the "either" resources
        val leftright = splitCosts(p)

        // get a list of all possible left/right combinations
        val combinations = List.fill(leftright.length)(List(0,1)).flatten combinations(leftright.length)

        // compute all possible combinations and their costs
        (combinations map{ combination =>
            val (l: List[Int], r: List[Int]) = combination.zip(leftright).map {
                case (0,(x,_)) => (x,0)
                case (1,(_,x)) => (0,x)
            }.unzip
            ( l.sum + costLeft, r.sum + costRight )
        } toList) distinct
    }

}

/* Trivial option to represent discarding of a card. Carries no information on
 * the actual card that is to be discarded.
 */
case class OptionDiscard extends PlayerOption {
    override def toString() = s"${Console.RED}~${Console.RESET} [Discard a card]"
}
