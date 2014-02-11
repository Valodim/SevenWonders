import scala.util.Random

import PlayerState.PlayerNumber

abstract class Wonder {
    val sides: List[WonderSide]
    def chooseSide(side: Int): WonderSide = sides(side)
}

abstract class WonderSide {

    // resources provided by the wonder
    val res = Resources()
    // stages that can be built
    val stages: List[WonderStage]

    /** An equivalent method to Card.categorize to retrieve if and how the
     * player may upgrade their wonder.
     * @see Card.categorize
     */
    def categorize(p: PlayerState, left: Resources, right: Resources): WonderOption = {
        // no stages left - too bad
        if ( p.wonderStuffed.length >= stages.length )
            return WonderFullyBuilt()

        val stage = stages(p.wonderStuffed.length)

        // calculate resources we don't have
        val req = stage.resourceReq - p.allResources
        // if we have everything - great
        if(req isEmpty)
            return WonderFree(stage)

        // check if the required resources minus the potentially available ones is empty
        if( (req - (left + right)) isEmpty) {
            val leftOnly = req - right
            val rightOnly = req - left

            // see if a trade option is viable
            val trade = WonderTrade(stage, req - leftOnly - rightOnly, leftOnly, rightOnly)

            // if it is, make the offer
            return if(trade.minCosts(p) <= p.gold)
                trade
            else
                // otherwise, too bad~
                WonderTradeInsufficientFunds(stage, req - leftOnly - rightOnly, leftOnly, rightOnly)
        }

        // otherwise - can't touch this
        WonderUnavailable(stage)
    }

    override def toString() = this.getClass.getSimpleName

}

object Wonder {
    val wonders = List( Babylon(), Halikarnassos(), Olympia(), Rhodos(), Ephesos(), Alexandria(), Gizah() )

    def newGameWonders(): List[Wonder] = Random.shuffle(wonders)
}

/* Abstract class for all stages of all wonders. This is similar to the Card
 * instance, yielding a benefit, worth and an additional endOfAgeBenefit (for
 * Babylon~, special little snowflake)
 */
abstract class WonderStage {
    val goldCost: Int = 0
    val resourceReq: Resources = Resources()
    // most commonly, wonder stages are worth some vp. we use this for a default case
    val value: Int = 0
    def benefit(p: PlayerState): (PlayerState, List[(PlayerNumber,LateAction)]) = p
    def endOfAgeBenefit(p: PlayerState): List[(PlayerNumber,LateAction)] = Nil
    def worth(p: PlayerState, g: GameState): Int = value

    override def toString() = this.getClass.getSimpleName
}

abstract class WonderOption extends PlayerOption {
    val stage: WonderStage
}
abstract class WonderAvailable extends WonderOption
case class WonderFree(stage: WonderStage) extends WonderAvailable {
    override def toString() = s"${Console.GREEN}+${Console.RESET} $stage"
}

case class WonderTrade(stage: WonderStage, either: Resources, left: Resources, right: Resources) extends WonderAvailable with TradeOption {
    override def toString() = s"${Console.YELLOW}+${Console.RESET} $stage [e${either.count}+l${left.count}+r${right.count}]"
}

case class WonderTradeInsufficientFunds(stage: WonderStage, either: Resources, left: Resources, right: Resources) extends WonderOption with TradeOption {
    override def toString() = s"${Console.YELLOW}—${Console.RESET} $stage [e${either.count}+l${left.count}+r${right.count}]"
}

case class WonderUnavailable(stage: WonderStage) extends WonderOption {
    override def toString() = s"${Console.RED}—${Console.RESET} $stage"
}

case class WonderFullyBuilt() extends WonderOption {
    // not sure if gusta...
    override val stage = null
    override def toString() = s"${Console.RED}—${Console.RESET} (wonder fully built)"
}

case class ActionPickOlympia(option: CardOption) extends Action {
    // play for free, and mark as used in this age
    def apply(p: PlayerState, g: GameState) = p.copy(wonder = p.wonder.asInstanceOf[OlympiaA].copy( specialLastUsed = g.age )) playForFree (option.card, g)
    def describe(p: PlayerState, g: GameState) = s"Player ${p.name} builds $option for free, courtesy of Olympia"
}

case class OptionOlympia extends PlayerOption {
    override def toString() = s"${Console.GREEN}!${Console.RESET} [Use Olympia's special]"
}

case class LateHalikarnassos() extends LateInteractiveAction {
    def describe(p: PlayerState, g: GameState) = s"${p.name} is going to retrieve a card from the discard pile, courtesy of Halikarnassos"
}
case class LateApplicableHalikarnassos(card: CardHalikarnassos) extends LateApplicableAction {
    override def apply(p: PlayerState, g: GameState) = p.playForFree(card.card, g)
    def describe(p: PlayerState, g: GameState) = s"${p.name} retrieves ${card.card} from discard pile, courtesy of Halikarnassos"
}
case class CardHalikarnassos(card: Card) extends CardFree {
    override def toString() = Console.BLUE + "! " + Console.RESET + card
}

case class LateBabylon() extends LateInteractiveAction {
    def describe(p: PlayerState, g: GameState) = s"${p.name} is going to build an extra card, courtesy of Babylon"
}
case class LateApplicableBabylon(a: Action) extends LateApplicableAction {
    override def apply(p: PlayerState, g: GameState) = a(p, g)
    def describe(p: PlayerState, g: GameState) = a.describe(p, g) + ", courtesy of Babylon"
}
