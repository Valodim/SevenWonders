// Halikarnassos Wonder
case class Halikarnassos extends Wonder {
    override val sides = List(HalikarnassosA(), HalikarnassosB())
}
case class HalikarnassosA extends WonderSide {
    override val res = Resources(cloth = 1)
    override val stages = List(HalikarnassosAStage1(), HalikarnassosAStage2(), HalikarnassosAStage3())
}
case class HalikarnassosB extends WonderSide {
    override val res = Resources(cloth = 1)
    override val stages = List(HalikarnassosBStage1(), HalikarnassosBStage2(), HalikarnassosBStage3())
}

case class HalikarnassosAStage1 extends WonderStage {
    override val value = 3
    override val resourceReq = Resources(clay = 2)
}
case class HalikarnassosAStage2 extends WonderStage {
    override val resourceReq = Resources(ore = 3)
    override def benefit(p: PlayerState) = (p, List((p.number, LateHalikarnassos())))
}
case class HalikarnassosAStage3 extends WonderStage {
    override val value = 7
    override val resourceReq = Resources(cloth = 2)
}
case class HalikarnassosBStage1 extends WonderStage {
    override val value = 2
    override val resourceReq = Resources(ore = 2)
    override def benefit(p: PlayerState) = (p, List((p.number, LateHalikarnassos())))
}
case class HalikarnassosBStage2 extends WonderStage {
    override val value = 1
    override val resourceReq = Resources(clay = 3)
    override def benefit(p: PlayerState) = (p, List((p.number, LateHalikarnassos())))
}
case class HalikarnassosBStage3 extends WonderStage {
    override val resourceReq = Resources(glass = 1, papyrus = 1, cloth = 1)
    override def benefit(p: PlayerState) = (p, List((p.number, LateHalikarnassos())))
}


// Olympia Wonder
case class Olympia extends Wonder {
    override val sides = List(OlympiaA(), OlympiaB())
}
case class OlympiaA(specialLastUsed: Int = 0) extends WonderSide {
    override val res = Resources(wood = 1)
    override val stages = List(OlympiaAStage1(), OlympiaAStage2(), OlympiaAStage3())
}
case class OlympiaB extends WonderSide {
    override val res = Resources(wood = 1)
    override val stages = List(OlympiaBStage1(), OlympiaBStage2(), OlympiaBStage3())
}

case class OlympiaAStage1 extends WonderStage {
    override val value = 3
    override val resourceReq = Resources(wood = 2)
}
case class OlympiaAStage2 extends WonderStage {
    override val resourceReq = Resources(stone = 2)
}
case class OlympiaAStage3 extends WonderStage {
    override val value = 7
    override val resourceReq = Resources(ore = 2)
}
case class OlympiaBStage1 extends WonderStage {
    override val resourceReq = Resources(wood = 2)
    override def benefit(p: PlayerState) = p.copy(
        tradeLeft = (1, p.tradeRight._2),
        tradeRight = (1, p.tradeRight._2)
    )
}
case class OlympiaBStage2 extends WonderStage {
    override val value = 5
    override val resourceReq = Resources(stone = 2)
}
case class OlympiaBStage3 extends WonderStage {
    override val resourceReq = Resources(ore = 2, cloth = 1)
    // todo: copy guild
}


// Babylon Wonder
case class Babylon extends Wonder {
    override val sides = List(BabylonA(), BabylonB())
}
case class BabylonA extends WonderSide {
    override val res = Resources(clay = 1)
    override val stages = List(BabylonAStage1(), BabylonAStage2(), BabylonAStage3())
}
case class BabylonB extends WonderSide {
    override val res = Resources(clay = 1)
    override val stages = List(BabylonBStage1(), BabylonBStage2(), BabylonBStage3())
}

case class BabylonAStage1 extends WonderStage {
    override val value = 3
    override val resourceReq = Resources(clay = 2)
}
case class BabylonAStage2 extends WonderStage {
    override val resourceReq = Resources(wood = 3)
    override def benefit(s: PlayerState) = s.copy(scienceWildCard = s.scienceWildCard + 1)
}
case class BabylonAStage3 extends WonderStage {
    override val value = 7
    override val resourceReq = Resources(clay = 4)
}
case class BabylonBStage1 extends WonderStage {
    override val value = 3
    override val resourceReq = Resources(clay = 1, cloth = 1)
}
case class BabylonBStage2 extends WonderStage {
    override val resourceReq = Resources(wood = 2, glass = 1)
    // resources CAN be used twice!
    // http://boardgamegeek.com/article/5739388#5739388
    override def endOfAgeBenefit(p: PlayerState) = List((p.number,LateBabylon()))
}
case class BabylonBStage3 extends WonderStage {
    override val resourceReq = Resources(clay = 3, papyrus = 1)
    override def benefit(s: PlayerState) = s.copy(scienceWildCard = s.scienceWildCard + 1)
}

// Ephesos Wonder
case class Ephesos extends Wonder {
    override val sides = List(EphesosA(), EphesosB())
}
case class EphesosA extends WonderSide {
    override val res = Resources(papyrus = 1)
    override val stages = List(EphesosAStage1(), EphesosAStage2(), EphesosAStage3())
}
case class EphesosB extends WonderSide {
    override val res = Resources(papyrus = 1)
    override val stages = List(EphesosBStage1(), EphesosBStage2(), EphesosBStage3())
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
    override val sides = List(AlexandriaA(), AlexandriaB())
}
case class AlexandriaA extends WonderSide {
    override val res = Resources(glass = 1)
    override val stages = List(AlexandriaAStage1(), AlexandriaAStage2(), AlexandriaAStage3())
}
case class AlexandriaB extends WonderSide {
    override val res = Resources(glass = 1)
    override val stages = List(AlexandriaBStage1(), AlexandriaBStage2(), AlexandriaBStage3())
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
    override val sides = List(RhodosA(), RhodosB())
}
case class RhodosA extends WonderSide {
    override val res = Resources(ore = 1)
    override val stages = List(RhodosAStage1(), RhodosAStage2(), RhodosAStage3())
}
case class RhodosB extends WonderSide {
    override val res = Resources(ore = 1)
    override val stages = List(RhodosBStage1(), RhodosBStage2())
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
    override val sides = List(GizahA(), GizahB())
}
case class GizahA extends WonderSide {
    override val res = Resources(stone = 1)
    override val stages = List(GizahAStage1(), GizahAStage2(), GizahAStage3())
}
case class GizahB extends WonderSide {
    override val res = Resources(stone = 1)
    override val stages = List(GizahBStage1(), GizahBStage2(), GizahBStage3(), GizahBStage4())
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
