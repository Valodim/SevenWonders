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
        if ( stages.length >= p.wonderStuffed.length )
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
            // TODO the "either" isn't quite right here
            return WonderTrade(stage, req - leftOnly - rightOnly, leftOnly, rightOnly)
        }

        // otherwise - can't touch this
        WonderUnavailable(stage)
    }

    override def toString() = this.getClass.getSimpleName

}

object Wonder {
    val wonders = List( Rhodos(), Ephesos(), Alexandria(), /* Babylon(), Olympia(), Halikarnassos(), */ Gizah() )

    def newGameWonders(): List[Wonder] = Random.shuffle(wonders)
}

abstract class WonderStage {
    val goldCost: Int = 0
    val resourceReq: Resources = Resources()
    // most commonly, wonder stages are worth some vp. we use this for a default case
    val value: Int = 0
    def benefit(p: PlayerState) = p
    def worth(p: PlayerState, g: GameState): Int = value
}


// Ephesos Wonder
case class Ephesos extends Wonder {
    override val res = Resources(papyrus = 1)
    override val stagesA = List(EphesosAStage1(), EphesosAStage2(), EphesosAStage3())
    override val stagesB = List(EphesosBStage1(), EphesosBStage2(), EphesosBStage3())
}

case class EphesosAStage1 extends WonderStage {
    override val value = 3
    override val resourceReq = Resources(stone = 2)
}
case class EphesosAStage2 extends WonderStage {
    override val resourceReq = Resources(wood = 2)
    override def benefit(s: PlayerState) = s.copy(gold = s.gold + 9)
}
case class EphesosAStage3 extends WonderStage {
    override val value = 7
    override val resourceReq = Resources(papyrus = 2)
}
case class EphesosBStage1 extends WonderStage {
    override val resourceReq = Resources(stone = 2)
    override val value = 2
    override def benefit(s: PlayerState) = s.copy(gold = s.gold + 4)
}
case class EphesosBStage2 extends WonderStage {
    override val resourceReq = Resources(wood = 2)
    override val value = 3
    override def benefit(s: PlayerState) = s.copy(gold = s.gold + 4)
}
case class EphesosBStage3 extends WonderStage {
    override val resourceReq = Resources(cloth = 1, glass = 1, papyrus = 1)
    override val value = 5
    override def benefit(s: PlayerState) = s.copy(gold = s.gold + 4)
}


// Alexandria Wonder
case class Alexandria extends Wonder {
    override val res = Resources(glass = 1)
    override val stagesA = List(AlexandriaAStage1(), AlexandriaAStage2(), AlexandriaAStage3())
    override val stagesB = List(AlexandriaBStage1(), AlexandriaBStage2(), AlexandriaBStage3())
}

case class AlexandriaAStage1 extends WonderStage {
    override val value = 3
    override val resourceReq = Resources(stone = 2)
}
case class AlexandriaAStage2 extends WonderStage {
    override val resourceReq = Resources(ore = 2)
    override def benefit(p: PlayerState) = p addNoTradeResources Resources.dynamic(wood = 1, stone = 1, clay = 1, ore = 1)
}
case class AlexandriaAStage3 extends WonderStage {
    override val value = 7
    override val resourceReq = Resources(glass = 2)
}
case class AlexandriaBStage1 extends WonderStage {
    override val resourceReq = Resources(clay = 2)
    override def benefit(p: PlayerState) = p addNoTradeResources Resources.dynamic(wood = 1, stone = 1, clay = 1, ore = 1)
}
case class AlexandriaBStage2 extends WonderStage {
    override val resourceReq = Resources(wood = 2)
    override def benefit(p: PlayerState) = p addNoTradeResources Resources.dynamic(papyrus = 1, glass = 1, cloth = 1)
}
case class AlexandriaBStage3 extends WonderStage {
    override val resourceReq = Resources(stone = 3)
    override val value = 7
}


// Rhodos wonder
case class Rhodos extends Wonder {
    override val res = Resources(wood = 1)
    override val stagesA = List(RhodosAStage1(), RhodosAStage2(), RhodosAStage3())
    override val stagesB = List(RhodosBStage1(), RhodosBStage2())
}


case class RhodosAStage1 extends WonderStage {
    override val value = 3
    override val resourceReq = Resources(wood = 2)
}
case class RhodosAStage2 extends WonderStage {
    override val resourceReq = Resources(clay = 3)
    override def benefit(s: PlayerState) = s.copy(shields = s.shields + 2)
}
case class RhodosAStage3 extends WonderStage {
    override val value = 7
    override val resourceReq = Resources(ore = 4)
}
case class RhodosBStage1 extends WonderStage {
    override val resourceReq = Resources(stone = 3)
    override val value = 3
    override def benefit(s: PlayerState) = s.copy(gold = s.gold + 3, shields = s.shields + 1)
}
case class RhodosBStage2 extends WonderStage {
    override val resourceReq = Resources(ore = 4)
    override val value = 4
    override def benefit(s: PlayerState) = s.copy(gold = s.gold + 4, shields = s.shields + 1)
}


// Gizah Wonder
case class Gizah extends Wonder {
    override val res = Resources(stone = 1)
    override val stagesA = List(GizahAStage1(), GizahAStage2(), GizahAStage3())
    override val stagesB = List(GizahBStage1(), GizahBStage2(), GizahBStage3(), GizahBStage4())
}

case class GizahAStage1 extends WonderStage {
    override val value = 3
    override val resourceReq = Resources(stone = 2)
}
case class GizahAStage2 extends WonderStage {
    override val value = 5
    override val resourceReq = Resources(wood = 3)
}
case class GizahAStage3 extends WonderStage {
    override val value = 7
    override val resourceReq = Resources(stone = 4)
}
case class GizahBStage1 extends WonderStage {
    override val value = 3
    override val resourceReq = Resources(wood = 2)
}
case class GizahBStage2 extends WonderStage {
    override val value = 5
    override val resourceReq = Resources(stone = 3)
}
case class GizahBStage3 extends WonderStage {
    override val value = 5
    override val resourceReq = Resources(clay = 3)
}
case class GizahBStage4 extends WonderStage {
    override val value = 7
    override val resourceReq = Resources(stone = 4, papyrus = 1)
}

abstract class WonderOption {
    val stage: WonderStage
}
abstract class WonderAvailable extends WonderOption
case class WonderFree(stage: WonderStage) extends WonderAvailable {
    override def toString() = s"${Console.GREEN}+ ${Console.RESET} $stage"
}

case class WonderTrade(stage: WonderStage, either: Resources, left: Resources, right: Resources) extends WonderOption {
    override def toString() = s"${Console.YELLOW}+ ${Console.RESET} $stage"
}

case class WonderInsufficientFunds(stage: WonderStage) extends WonderOption {
    override def toString() = s"${Console.RED}— ${Console.RESET} $stage"
}

case class WonderUnavailable(stage: WonderStage) extends WonderOption {
    override def toString() = s"${Console.RED}— ${Console.RESET} $stage"
}

case class WonderFullyBuilt() extends WonderOption {
    // not sure if gusta...
    override val stage = null
    override def toString() = s"${Console.RED}— ${Console.RESET} $stage"
}

