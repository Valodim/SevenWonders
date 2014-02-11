import scala.util.Random

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

class DumbAI extends AI {
    // dummy AI
    def chooseAction(p: PlayerState, g: GameState) = {
        p.hand.cards.head.categorize(p, Resources(), Resources()) match {
            case card: CardFree => Some(ActionPick(card))
            case card => Some(ActionDiscard(card))
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

object AI {

    def random: AI = {
        new DumbAI()
    }

}
