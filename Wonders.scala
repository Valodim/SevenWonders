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
}

object Wonder {
    val wonders = List( Rhodos(), Ephesos(), /* Alexandria(), Babylon(), Olympia(), Halikarnassos(), */ Gizah() )

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
