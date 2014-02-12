import scala.util.Random

/* An Interface provides methods for all interaction for a single player. This
 * includes both interactive and non-interactive (ie, cpu player) instances.
 */
abstract class Interface {

    // Yields a WonderSide for a given Wonder, or None on bad input
    def chooseWonderSide(wonder: Wonder): Option[WonderSide]

    // Yields an Action for a given player in a given GameState, or None on bad input
    def chooseAction(p: PlayerState, g: GameState): Option[Action]

    // Do special action for Babylon
    def specialBabylon(p: PlayerState, g: GameState): Option[LateApplicableBabylon]

    // Search discard pile for a Halikarnassos draw
    def specialHalikarnassos(p: PlayerState, g: GameState, newDiscards: List[Card]): Option[LateApplicableHalikarnassos]

    def persistRequest[T,S](a: (T => Option[S]))(p: T) = {
        var result: Option[S] = None
        while(result == None)
            result = a(p)
        result.get
    }
    // def persistWonderSide(wonder: Wonder) = persistRequest(chooseWonderSide) _
    // def persistAction(p: PlayerState, g: GameState) = persistRequest(chooseAction) _
    // def persistBabylon(p: PlayerState, g: GameState) = persistRequest(specialBabylon) _
    // def persistHalikarnassos(p: PlayerState, g: GameState, newDiscards: List[Card]) = persistRequest(specialHalikarnassos.curry) _

}


// a non-interactive interface
abstract class AI extends Interface

/* Completely dumb AI. Always picks first card, discards if it can't be played. */
class DumbAI extends AI {
    def chooseAction(p: PlayerState, g: GameState) = {
        p.hand.cards.head.categorize(p, Resources(), Resources()) match {
            case option: CardFree => Some(ActionPick(option))
            case option => Some(ActionDiscard(option))
        }
    }

    // Yields a WonderSide for a given Wonder, or None on bad input
    def chooseWonderSide(wonder: Wonder): Option[WonderSide] = {
        Some(wonder.sides(Random.nextInt(wonder.sides.length)))
    }

    // Do special action for Babylon
    def specialBabylon(p: PlayerState, g: GameState): Option[LateApplicableBabylon] = None

    // Search discard pile for a Halikarnassos draw
    def specialHalikarnassos(p: PlayerState, g: GameState, newDiscards: List[Card]): Option[LateApplicableHalikarnassos] = None

}

/* Slightly less dumb AI - picks any available option randomly, including
 * (random) trades.
 */
class SlightlyLessDumbAI extends AI {
    def chooseAction(p: PlayerState, g: GameState) = {
        Random.shuffle(p.hand.cards) map ( _.categorize(p, (p lefty g).resources, (p righty g).resources) ) collectFirst {
            case o: CardFree => ActionPick(o)
            case o: CardTrade => decideTrade(p, g, o) tradeOffer(p, o) getOrElse {
                println("bad trade? this is a bug :(")
                ActionDiscard(o)
            }
        } orElse {
            Some(ActionDiscard(p.hand.cards(Random.nextInt(p.hand.cards.length))))
        }
    }

    // Yields a WonderSide for a given Wonder, or None on bad input
    def chooseWonderSide(wonder: Wonder): Option[WonderSide] = {
        Some(wonder.sides(Random.nextInt(wonder.sides.length)))
    }

    // We don't build wonders so this can never happen
    def specialBabylon(p: PlayerState, g: GameState): Option[LateApplicableBabylon] = None

    // We don't build wonders so this can never happen
    def specialHalikarnassos(p: PlayerState, g: GameState, newDiscards: List[Card]): Option[LateApplicableHalikarnassos] = None

    def decideTrade(p: PlayerState, g: GameState, t: TradeOption): Trade = {
        // random trade option
        val costs = t.allCosts(p)
        val pick = costs(Random.nextInt(costs.length))
        Trade(pick._1, pick._2)
    }

}


/* Specialized AI, always picks the option with maximum VP value.
 */
class SimpleGreedyAI extends AI {
    def chooseAction(p: PlayerState, g: GameState) = {
        val options = p.hand.cards map ( _.categorize(p, (p lefty g).resources, (p righty g).resources) ) collect {
            case o: CardFree => ActionPick(o)
            case o: CardTrade => decideTrade(p, g, o) tradeOffer(p, o) getOrElse {
                println("bad trade? this is a bug :(")
                ActionDiscard(o)
            }
        }
        if(options.isEmpty)
            Some(ActionDiscard(p.hand.cards.head))
        else
            Some(options maxBy( action => action(p, g)._1 totalvp(g) ))
    }

    // Yields a WonderSide for a given Wonder, or None on bad input
    def chooseWonderSide(wonder: Wonder): Option[WonderSide] = {
        Some(wonder.sides(Random.nextInt(wonder.sides.length)))
    }

    // We don't build wonders so this can never happen
    def specialBabylon(p: PlayerState, g: GameState): Option[LateApplicableBabylon] = None

    // We don't build wonders so this can never happen
    def specialHalikarnassos(p: PlayerState, g: GameState, newDiscards: List[Card]): Option[LateApplicableHalikarnassos] = None

    // Just pay the minimum costs
    def decideTrade(p: PlayerState, g: GameState, t: TradeOption): Trade = {
        // pick minimum costs, with a small penalty for lopsided distribution
        val costs = t.allCosts(p) minBy ( x => (x._1 + x._2) + 0.1*(x._1-x._2).abs )
        Trade(costs._1, costs._2)
    }

}


object AI {

    def random: AI = {
        new SimpleGreedyAI()
    }

}
