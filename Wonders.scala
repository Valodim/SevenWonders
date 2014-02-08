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
