// Card types
abstract class BrownOrGreyCard extends Card {
    val res: Resources
    override def benefit(p: PlayerState, g: GameState) = p addResources res
}
abstract class BrownCard extends BrownOrGreyCard {
    override def toString = SevenCli.brown + super.toString + Console.RESET
}
abstract class GreyCard extends BrownOrGreyCard {
    override def toString = SevenCli.grey + super.toString + Console.RESET
}
abstract class RedCard extends Card {
    val value: Int
    override def benefit(p: PlayerState, g: GameState) = p.copy(shields = p.shields + value)
    override def toString = SevenCli.red + super.toString + Console.RESET
}
abstract class GreenCard extends Card {
    val value: (Int,Int,Int)
    override def benefit(p: PlayerState, g: GameState) = p.copy(
        science = (
            p.science._1 + value._1,
            p.science._2 + value._2,
            p.science._3 + value._3
        )
    )
    override def toString = SevenCli.green + super.toString + Console.RESET
}
abstract class BlueCard extends Card {
    val value: Int
    override def worth(p: PlayerState, g: GameState) = value
    override def toString = SevenCli.blue + super.toString + Console.RESET
}
abstract class YellowCard extends Card {
    override def toString = SevenCli.yellow + super.toString + Console.RESET
}
abstract class PurpleCard extends Card {
    override def toString = SevenCli.purple + super.toString + Console.RESET
}

// age 1 brown cards
case object WoodPlace extends BrownCard {
    override val res = Resources(wood = 1)
}
case object ClayPlace extends BrownCard {
    override val res = Resources(clay = 1)
}
case object OrePlace extends BrownCard {
    override val res = Resources(ore = 1)
}
case object StonePlace extends BrownCard {
    override val res = Resources(stone = 1)
}
case object Baumschule extends BrownCard {
    override val goldCost = 1
    override val res = Resources.dynamic(wood = 1, clay = 1)
}
case object Ausgrabungsstätte extends BrownCard {
    override val goldCost = 1
    override val res = Resources.dynamic(stone = 1, clay = 1)
}
case object Tongrube extends BrownCard {
    override val goldCost = 1
    override val res = Resources.dynamic(clay = 1, ore = 1)
}
case object Forstwirtschaft extends BrownCard {
    override val goldCost = 1
    override val res = Resources.dynamic(wood = 1, stone =1)
}
case object Waldhöhle extends BrownCard {
    override val goldCost = 1
    override val res = Resources.dynamic(wood = 1, ore = 1)
}
case object Mine extends BrownCard {
    override val goldCost = 1
    override val res = Resources.dynamic(stone = 1, ore = 1)
}

// age 2 brown cards
case object Sägewerk extends BrownCard {
    override val goldCost = 1
    override val res = Resources(wood = 2)
}
case object Bildhauerei extends BrownCard {
    override val goldCost = 1
    override val res = Resources(stone = 2)
}
case object Ziegelbrennerei extends BrownCard {
    override val goldCost = 1
    override val res = Resources(clay = 2)
}
case object Giesserei extends BrownCard {
    override val goldCost = 1
    override val res = Resources(ore = 2)
}


// age 1 + 2 grey cards
case object Press extends GreyCard {
    override val res = Resources(papyrus = 1)
}
case object Weavery extends GreyCard {
    override val res = Resources(cloth = 1)
}
case object Glassery extends GreyCard {
    override val res = Resources(glass = 1)
}


// age 1 blue cards
case object Pfandhaus extends BlueCard {
    override val value = 3
}
case object Bäder extends BlueCard {
    override val resourceReq = Resources(stone = 1)
    override val value = 3
    override val chains = List(Aquädukt)
}
case object Altar extends BlueCard {
    override val value = 2
    override val chains = List(Tempel)
}
case object Theatre extends BlueCard {
    override val value = 2
    override val chains = List(Statue)
}

// age 2 blue cards
case object Aquädukt extends BlueCard {
    override val value = 5
    override val resourceReq = Resources(stone = 3)
}
case object Tempel extends BlueCard {
    override val value = 3
    override val resourceReq = Resources(wood = 1, clay = 1, glass = 1)
    override val chains = List(Pantheon)
}
case object Statue extends BlueCard {
    override val value = 4
    override val resourceReq = Resources(ore = 2, wood = 1)
    override val chains = List(Gärten)
}
case object Gericht extends BlueCard {
    override val value = 4
    override val resourceReq = Resources(clay = 2, cloth = 1)
}

// age 3 blue cards
case object Pantheon extends BlueCard {
    override val value = 7
    override val resourceReq = Resources(clay = 2, ore = 1, glass = 1, papyrus = 1, cloth = 1)
}
case object Gärten extends BlueCard {
    override val value = 5
    override val resourceReq = Resources(clay = 2, wood = 1)
}
case object Rathaus extends BlueCard {
    override val value = 6
    override val resourceReq = Resources(stone = 2, ore = 1, glass = 1)
}
case object Palast extends BlueCard {
    override val value = 8
    override val resourceReq = Resources(wood = 1, stone = 1, clay = 1, ore = 1, glass = 1, papyrus = 1, cloth = 1)
}
case object Senat extends BlueCard {
    override val value = 6
    override val resourceReq = Resources(wood = 2, stone = 1, ore = 1)
}



// age 1 yellow cards
case object Tavern extends YellowCard {
    override def benefit(p: PlayerState, g: GameState) = p addGold 5
}
case object KontorOst extends YellowCard {
    override def benefit(p: PlayerState, g: GameState) = p.copy(tradeRight = (1, p.tradeRight._2))
}
case object KontorWest extends YellowCard {
    override def benefit(p: PlayerState, g: GameState) = p.copy(tradeLeft = (1, p.tradeRight._2))
}
case object Market extends YellowCard {
    override def benefit(p: PlayerState, g: GameState) = p.copy(
        tradeLeft = (p.tradeRight._1, 1),
        tradeRight = (p.tradeRight._1, 1)
    )
}

// age 2 yellow cards
case object Forum extends YellowCard {
    override val resourceReq = Resources(clay = 2)
    override def benefit(p: PlayerState, g: GameState) = p addNoTradeResources Resources.dynamic(cloth = 1, glass = 1, papyrus = 1)
    override val chains = List(Hafen)
}
case object Karawanserei extends YellowCard {
    override val resourceReq = Resources(wood = 2)
    override def benefit(p: PlayerState, g: GameState) = p addNoTradeResources Resources.dynamic(wood = 1, stone = 1, ore = 1, clay = 1)
    override val chains = List(Leuchtturm)
}
case object Weinberg extends YellowCard {
    override def benefit(p: PlayerState, g: GameState) = p addGold( p countAll( _.isInstanceOf[BrownCard], g) )
}
case object Basar extends YellowCard {
    override def benefit(p: PlayerState, g: GameState) = p addGold( 2 * p.countAll( _.isInstanceOf[GreyCard], g) )
}

// age 3 yellow cards
case object Hafen extends YellowCard {
    override val resourceReq = Resources(wood = 1, ore = 1, cloth = 1)
    override def benefit(p: PlayerState, g: GameState) = p addGold p.count( _.isInstanceOf[BrownCard] )
    override def worth(p: PlayerState, g: GameState) = p.cards.count( _.isInstanceOf[BrownCard] )
}
case object Leuchtturm extends YellowCard {
    override val resourceReq = Resources(stone = 1, glass = 1)
    override def benefit(p: PlayerState, g: GameState) = p addGold p.count( _.isInstanceOf[YellowCard] )
    override def worth(p: PlayerState, g: GameState) = p.cards.count( _.isInstanceOf[YellowCard] )
}
case object Handelskammer extends YellowCard {
    override val resourceReq = Resources(clay = 2, papyrus = 1)
    override def benefit(p: PlayerState, g: GameState) = p addGold(2 * p.count( _.isInstanceOf[GreyCard] ) )
    override def worth(p: PlayerState, g: GameState) = 2 * p.count( _.isInstanceOf[GreyCard] )
}
case object Arena extends YellowCard {
    override val resourceReq = Resources(stone = 2, ore = 1)
    override def benefit(p: PlayerState, g: GameState) = p addGold(3 * p.wonderStuffed.length)
    override def worth(p: PlayerState, g: GameState) = p.wonderStuffed.length
}


// age 1 red cards
case object Befestigungsanlage extends RedCard {
    override val value = 1
    override val resourceReq = Resources(wood = 1)
}
case object Kaserne extends RedCard {
    override val value = 1
    override val resourceReq = Resources(ore = 1)
}
case object Wachturm extends RedCard {
    override val value = 1
    override val resourceReq = Resources(clay = 1)
}

// age 2 red cards
case object Mauern extends RedCard {
    override val value = 2
    override val resourceReq = Resources(stone = 3)
    override val chains = List(Verteidigungsanlage)
}
case object Trainingsgelände extends RedCard {
    override val value = 2
    override val resourceReq = Resources(ore = 2, wood = 1)
    override val chains = List(Zirkus)
}
case object Ställe extends RedCard {
    override val value = 2
    override val resourceReq = Resources(clay = 1, wood = 1, ore = 1)
}
case object Schiessplatz extends RedCard {
    override val value = 2
    override val resourceReq = Resources(wood = 2, ore = 1)
}

// age 3 red cards
case object Verteidigungsanlage extends RedCard {
    override val value = 3
    override val resourceReq = Resources(stone = 1, ore = 3)
}
case object Zirkus extends RedCard {
    override val value = 3
    override val resourceReq = Resources(stone = 3, ore = 1)
}
case object Waffenlager extends RedCard {
    override val value = 3
    override val resourceReq = Resources(wood = 2, ore = 1, cloth = 1)
}
case object Belagerungsmaschinen extends RedCard {
    override val value = 3
    override val resourceReq = Resources(clay = 3, wood = 1)
}



// age 1 green cards
case object Apothecary extends GreenCard {
    override val resourceReq = Resources(cloth = 1)
    override val chains = List(Arzneiausgabe, Ställe)
    override val value = (1,0,0)
}
case object Werkstatt extends GreenCard {
    override val resourceReq = Resources(glass = 1)
    override val chains = List(Laboratorium, Schiessplatz)
    override val value = (0,1,0)
}
case object Skriptorium extends GreenCard {
    override val resourceReq = Resources(papyrus = 1)
    override val chains = List(Bibliothek, Gericht)
    override val value = (0,0,1)
}

// age 2 green cards
case object Arzneiausgabe extends GreenCard {
    override val resourceReq = Resources(ore = 2, glass = 1)
    override val value = (1,0,0)
    override val chains = List(Loge, Arena)
}
case object Laboratorium extends GreenCard {
    override val resourceReq = Resources(clay = 2, papyrus = 1)
    override val value = (0,1,0)
    override val chains = List(Belagerungsmaschinen, Observatorium)
}
case object Bibliothek extends GreenCard {
    override val resourceReq = Resources(stone = 2, cloth = 1)
    override val value = (0,0,1)
    override val chains = List(Senat, Universität)
}
case object Schule extends GreenCard {
    override val resourceReq = Resources(wood = 1, papyrus = 1)
    override val value = (0,0,1)
    override val chains = List(Akademie, Studierzimmer)
}

// age 3 green cards
case object Loge extends GreenCard {
    override val resourceReq = Resources(clay = 2, papyrus = 1, cloth = 1)
    override val value = (1,0,0)
}
case object Observatorium extends GreenCard {
    override val resourceReq = Resources(ore = 2, glass = 1, cloth = 1)
    override val value = (0,1,0)
}
case object Universität extends GreenCard {
    override val resourceReq = Resources(wood = 2, papyrus = 1, glass = 1)
    override val value = (0,0,1)
    override val chains = List(Senat)
}
case object Akademie extends GreenCard {
    override val resourceReq = Resources(stone = 3, glass = 1)
    override val value = (1,0,0)
}
case object Studierzimmer extends GreenCard {
    override val resourceReq = Resources(wood = 1, papyrus = 1, cloth = 1)
    override val value = (0,1,0)
}


// age 3 purple cards
case object GuildWorkers extends PurpleCard {
    override val resourceReq = Resources(ore = 2, clay = 1, stone = 1, wood = 1)
    override def worth(p: PlayerState, g: GameState) = p countNeighbors( _.isInstanceOf[BrownCard], g)
}
case object GuildArtisans extends PurpleCard {
    override val resourceReq = Resources(ore = 2, stone = 2)
    override def worth(p: PlayerState, g: GameState) = 2 * p.countNeighbors( _.isInstanceOf[GreyCard], g)
}
case object GuildTraders extends PurpleCard {
    override val resourceReq = Resources(glass = 1, papyrus = 1, cloth = 1)
    override def worth(p: PlayerState, g: GameState) = p countNeighbors( _.isInstanceOf[YellowCard], g)
}
case object GuildPhilosophy extends PurpleCard {
    override val resourceReq = Resources(clay = 3, papyrus = 1, cloth = 1)
    override def worth(p: PlayerState, g: GameState) = p countNeighbors( _.isInstanceOf[GreenCard], g)
}
case object GuildSpies extends PurpleCard {
    override val resourceReq = Resources(clay = 3, glass = 1)
    override def worth(p: PlayerState, g: GameState) = p countNeighbors( _.isInstanceOf[RedCard], g)
}
case object GuildStrategists extends PurpleCard {
    override val resourceReq = Resources(ore = 2, stone = 1, cloth = 1)
    override def worth(p: PlayerState, g: GameState) = ((p lefty g).redlost + (p righty g).redlost) abs
}
case object GuildReeder extends PurpleCard {
    override val resourceReq = Resources(wood = 3, glass = 1, papyrus = 1)
    override def worth(p: PlayerState, g: GameState) = p count(x => x.isInstanceOf[BrownOrGreyCard] || x.isInstanceOf[PurpleCard])
}
case object GuildScientists extends PurpleCard {
    override val resourceReq = Resources(wood = 2, ore = 2, papyrus = 1)
    override def benefit(p: PlayerState, g: GameState) = p.copy(scienceWildCard = p.scienceWildCard+1)
}
case object GuildOfficials extends PurpleCard {
    override val resourceReq = Resources(wood = 3, stone = 1, cloth = 1)
    override def worth(p: PlayerState, g: GameState) = p countNeighbors( _.isInstanceOf[BlueCard], g)
}
case object GuildBuilders extends PurpleCard {
    override val resourceReq = Resources(stone = 2, clay = 2, glass = 1)
    override def worth(p: PlayerState, g: GameState) = p.wonderStuffed.length + (p lefty g).wonderStuffed.length + (p righty g).wonderStuffed.length
}
