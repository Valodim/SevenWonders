// Halikarnassos Wonder
case object Halikarnassos extends Wonder {
    override val sides = List(HalikarnassosA, HalikarnassosB)
}
case object HalikarnassosA extends WonderSide {
    override val res = Resources(cloth = 1)
    override val stages = List(HalikarnassosAStage1, HalikarnassosAStage2, HalikarnassosAStage3)
}
case object HalikarnassosB extends WonderSide {
    override val res = Resources(cloth = 1)
    override val stages = List(HalikarnassosBStage1, HalikarnassosBStage2, HalikarnassosBStage3)
}

case object HalikarnassosAStage1 extends WonderStage {
    override val value = 3
    override val resourceReq = Resources(clay = 2)
}
case object HalikarnassosAStage2 extends WonderStage {
    override val resourceReq = Resources(ore = 3)
    override def benefit(p: PlayerState) = (p, List((p.number, LateHalikarnassos)))
}
case object HalikarnassosAStage3 extends WonderStage {
    override val value = 7
    override val resourceReq = Resources(cloth = 2)
}
case object HalikarnassosBStage1 extends WonderStage {
    override val value = 2
    override val resourceReq = Resources(ore = 2)
    override def benefit(p: PlayerState) = (p, List((p.number, LateHalikarnassos)))
}
case object HalikarnassosBStage2 extends WonderStage {
    override val value = 1
    override val resourceReq = Resources(clay = 3)
    override def benefit(p: PlayerState) = (p, List((p.number, LateHalikarnassos)))
}
case object HalikarnassosBStage3 extends WonderStage {
    override val resourceReq = Resources(glass = 1, papyrus = 1, cloth = 1)
    override def benefit(p: PlayerState) = (p, List((p.number, LateHalikarnassos)))
}


// Olympia Wonder
case object Olympia extends Wonder {
    override val sides = List(OlympiaA(), OlympiaB)
}
case class OlympiaA(specialLastUsed: Int = 0) extends WonderSide {
    override val res = Resources(wood = 1)
    override val stages = List(OlympiaAStage1, OlympiaAStage2, OlympiaAStage3)
}
case object OlympiaB extends WonderSide {
    override val res = Resources(wood = 1)
    override val stages = List(OlympiaBStage1, OlympiaBStage2, OlympiaBStage3)
}

case object OlympiaAStage1 extends WonderStage {
    override val value = 3
    override val resourceReq = Resources(wood = 2)
}
case object OlympiaAStage2 extends WonderStage {
    override val resourceReq = Resources(stone = 2)
}
case object OlympiaAStage3 extends WonderStage {
    override val value = 7
    override val resourceReq = Resources(ore = 2)
}
case object OlympiaBStage1 extends WonderStage {
    override val resourceReq = Resources(wood = 2)
    override def benefit(p: PlayerState) = p.copy(
        tradeLeft = (1, p.tradeRight._2),
        tradeRight = (1, p.tradeRight._2)
    )
}
case object OlympiaBStage2 extends WonderStage {
    override val value = 5
    override val resourceReq = Resources(stone = 2)
}
case object OlympiaBStage3 extends WonderStage {
    override val resourceReq = Resources(ore = 2, cloth = 1)
    override def worth(p: PlayerState, g: GameState): Int = {
        val guilds = (p lefty g).filter(_.isInstanceOf[PurpleCard]) ++ (p righty g).filter(_.isInstanceOf[PurpleCard])
        if(guilds.isEmpty) {
            println("Olympia: no guild to copy :(")
            return 0
        }
        val card = guilds maxBy ( _.worth(p, g) )
        val points = card.worth(p, g)
        println(s"Olympia: copying $card for $points VP")
        points
    }
}


// Babylon Wonder
case object Babylon extends Wonder {
    override val sides = List(BabylonA, BabylonB)
}
case object BabylonA extends WonderSide {
    override val res = Resources(clay = 1)
    override val stages = List(BabylonAStage1, BabylonAStage2, BabylonAStage3)
}
case object BabylonB extends WonderSide {
    override val res = Resources(clay = 1)
    override val stages = List(BabylonBStage1, BabylonBStage2, BabylonBStage3)
}

case object BabylonAStage1 extends WonderStage {
    override val value = 3
    override val resourceReq = Resources(clay = 2)
}
case object BabylonAStage2 extends WonderStage {
    override val resourceReq = Resources(wood = 3)
    override def benefit(s: PlayerState) = s.copy(scienceWildCard = s.scienceWildCard + 1)
}
case object BabylonAStage3 extends WonderStage {
    override val value = 7
    override val resourceReq = Resources(clay = 4)
}
case object BabylonBStage1 extends WonderStage {
    override val value = 3
    override val resourceReq = Resources(clay = 1, cloth = 1)
}
case object BabylonBStage2 extends WonderStage {
    override val resourceReq = Resources(wood = 2, glass = 1)
    // resources CAN be used twice!
    // http://boardgamegeek.com/article/5739388#5739388
    override def endOfAgeBenefit(p: PlayerState) = List((p.number,LateBabylon))
}
case object BabylonBStage3 extends WonderStage {
    override val resourceReq = Resources(clay = 3, papyrus = 1)
    override def benefit(s: PlayerState) = s.copy(scienceWildCard = s.scienceWildCard + 1)
}

// Ephesos Wonder
case object Ephesos extends Wonder {
    override val sides = List(EphesosA, EphesosB)
}
case object EphesosA extends WonderSide {
    override val res = Resources(papyrus = 1)
    override val stages = List(EphesosAStage1, EphesosAStage2, EphesosAStage3)
}
case object EphesosB extends WonderSide {
    override val res = Resources(papyrus = 1)
    override val stages = List(EphesosBStage1, EphesosBStage2, EphesosBStage3)
}

case object EphesosAStage1 extends WonderStage {
    override val value = 3
    override val resourceReq = Resources(stone = 2)
}
case object EphesosAStage2 extends WonderStage {
    override val resourceReq = Resources(wood = 2)
    override def benefit(s: PlayerState) = s.copy(gold = s.gold + 9)
}
case object EphesosAStage3 extends WonderStage {
    override val value = 7
    override val resourceReq = Resources(papyrus = 2)
}
case object EphesosBStage1 extends WonderStage {
    override val resourceReq = Resources(stone = 2)
    override val value = 2
    override def benefit(s: PlayerState) = s.copy(gold = s.gold + 4)
}
case object EphesosBStage2 extends WonderStage {
    override val resourceReq = Resources(wood = 2)
    override val value = 3
    override def benefit(s: PlayerState) = s.copy(gold = s.gold + 4)
}
case object EphesosBStage3 extends WonderStage {
    override val resourceReq = Resources(cloth = 1, glass = 1, papyrus = 1)
    override val value = 5
    override def benefit(s: PlayerState) = s.copy(gold = s.gold + 4)
}


// Alexandria Wonder
case object Alexandria extends Wonder {
    override val sides = List(AlexandriaA, AlexandriaB)
}
case object AlexandriaA extends WonderSide {
    override val res = Resources(glass = 1)
    override val stages = List(AlexandriaAStage1, AlexandriaAStage2, AlexandriaAStage3)
}
case object AlexandriaB extends WonderSide {
    override val res = Resources(glass = 1)
    override val stages = List(AlexandriaBStage1, AlexandriaBStage2, AlexandriaBStage3)
}

case object AlexandriaAStage1 extends WonderStage {
    override val value = 3
    override val resourceReq = Resources(stone = 2)
}
case object AlexandriaAStage2 extends WonderStage {
    override val resourceReq = Resources(ore = 2)
    override def benefit(p: PlayerState) = p addNoTradeResources Resources.dynamic(wood = 1, stone = 1, clay = 1, ore = 1)
}
case object AlexandriaAStage3 extends WonderStage {
    override val value = 7
    override val resourceReq = Resources(glass = 2)
}
case object AlexandriaBStage1 extends WonderStage {
    override val resourceReq = Resources(clay = 2)
    override def benefit(p: PlayerState) = p addNoTradeResources Resources.dynamic(wood = 1, stone = 1, clay = 1, ore = 1)
}
case object AlexandriaBStage2 extends WonderStage {
    override val resourceReq = Resources(wood = 2)
    override def benefit(p: PlayerState) = p addNoTradeResources Resources.dynamic(papyrus = 1, glass = 1, cloth = 1)
}
case object AlexandriaBStage3 extends WonderStage {
    override val resourceReq = Resources(stone = 3)
    override val value = 7
}


// Rhodos wonder
case object Rhodos extends Wonder {
    override val sides = List(RhodosA, RhodosB)
}
case object RhodosA extends WonderSide {
    override val res = Resources(ore = 1)
    override val stages = List(RhodosAStage1, RhodosAStage2, RhodosAStage3)
}
case object RhodosB extends WonderSide {
    override val res = Resources(ore = 1)
    override val stages = List(RhodosBStage1, RhodosBStage2)
}


case object RhodosAStage1 extends WonderStage {
    override val value = 3
    override val resourceReq = Resources(wood = 2)
}
case object RhodosAStage2 extends WonderStage {
    override val resourceReq = Resources(clay = 3)
    override def benefit(s: PlayerState) = s.copy(shields = s.shields + 2)
}
case object RhodosAStage3 extends WonderStage {
    override val value = 7
    override val resourceReq = Resources(ore = 4)
}
case object RhodosBStage1 extends WonderStage {
    override val resourceReq = Resources(stone = 3)
    override val value = 3
    override def benefit(s: PlayerState) = s.copy(gold = s.gold + 3, shields = s.shields + 1)
}
case object RhodosBStage2 extends WonderStage {
    override val resourceReq = Resources(ore = 4)
    override val value = 4
    override def benefit(s: PlayerState) = s.copy(gold = s.gold + 4, shields = s.shields + 1)
}


// Gizah Wonder
case object Gizah extends Wonder {
    override val sides = List(GizahA, GizahB)
}
case object GizahA extends WonderSide {
    override val res = Resources(stone = 1)
    override val stages = List(GizahAStage1, GizahAStage2, GizahAStage3)
}
case object GizahB extends WonderSide {
    override val res = Resources(stone = 1)
    override val stages = List(GizahBStage1, GizahBStage2, GizahBStage3, GizahBStage4)
}

case object GizahAStage1 extends WonderStage {
    override val value = 3
    override val resourceReq = Resources(stone = 2)
}
case object GizahAStage2 extends WonderStage {
    override val value = 5
    override val resourceReq = Resources(wood = 3)
}
case object GizahAStage3 extends WonderStage {
    override val value = 7
    override val resourceReq = Resources(stone = 4)
}
case object GizahBStage1 extends WonderStage {
    override val value = 3
    override val resourceReq = Resources(wood = 2)
}
case object GizahBStage2 extends WonderStage {
    override val value = 5
    override val resourceReq = Resources(stone = 3)
}
case object GizahBStage3 extends WonderStage {
    override val value = 5
    override val resourceReq = Resources(clay = 3)
}
case object GizahBStage4 extends WonderStage {
    override val value = 7
    override val resourceReq = Resources(stone = 4, papyrus = 1)
}
