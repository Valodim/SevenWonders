import scala.util.Random

abstract class Wonder {
    // the side picked, A = 1, B = 2. may be changed by the player, but only at
    // the beginning of the game. all other wonder-state is kept with the player.
    val side = 1

    // resources provided by the wonder
    val res = Resources()
    val stagesA: List[WonderStage]
    val stagesB: List[WonderStage]

    lazy val stages = if(side == 1) stagesA else stagesB

    /** Central method for categorizing if and how a player can play a card.
     * This is the main place where resource, gold and other requirements for
     * cards are checked.
     *
     * @returns A CardOption instance, which is required to instantiate a
     * PickAction object, which is the only way a player can play cards.
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
    val wonders = List( Rhodos(), Ephesos(), Alexandria(), Babylon(), Olympia(), Halikarnassos(), Gizah() )

    def newGameWonders(): List[Wonder] = Random.shuffle(wonders)
}

abstract class WonderStage {
    val goldCost: Int = 0
    val resourceReq: Resources = Resources()
    // most commonly, wonder stages are worth some vp. we use this for a default case
    val value: Int = 0
    def benefit(p: PlayerState) = p
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
    def apply(p: PlayerState, g: GameState) =
        (p.copy(wonder = p.wonder.asInstanceOf[Olympia].copy( specialLastUsed = g.age )) playForFree (option.card, g), None, Nil)
    def describe(p: PlayerState, g: GameState) = s"Player ${p.name} builds $option for free, courtesy of Olympia"
}

case class OptionOlympia extends PlayerOption {
    override def toString() = s"${Console.GREEN}!${Console.RESET} [Use Olympia's special]"
}
