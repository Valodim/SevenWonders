abstract class BrownOrGreyCard extends Card {
    val res: Resources
    override def benefit(p: PlayerState, g: GameState) = p addResources res
}
abstract class BrownCard() extends BrownOrGreyCard {
    override def toString() = SevenCli.brown + this.getClass.getSimpleName + Console.RESET
}
abstract class GreyCard() extends BrownOrGreyCard {
    override def toString() = SevenCli.grey + this.getClass.getSimpleName + Console.RESET
}
abstract class RedCard() extends Card {
    val value: Int
    override def benefit(p: PlayerState, g: GameState) = p.copy(shields = p.shields + value)
    override def toString() = SevenCli.red + this.getClass.getSimpleName + Console.RESET
}
abstract class GreenCard() extends Card {
    val value: (Int,Int,Int)
    override def benefit(p: PlayerState, g: GameState) = p.copy(
        science = (
            p.science._1 + value._1,
            p.science._2 + value._2,
            p.science._3 + value._3
        )
    )
    override def toString() = SevenCli.green + this.getClass.getSimpleName + Console.RESET
}
abstract class BlueCard() extends Card {
    val value: Int
    override def worth(p: PlayerState, g: GameState) = value
    override def toString() = SevenCli.blue + this.getClass.getSimpleName + Console.RESET
}
abstract class YellowCard() extends Card {
    override def toString() = SevenCli.yellow + this.getClass.getSimpleName + Console.RESET
}
abstract class PurpleCard() extends Card {
    override def toString() = SevenCli.purple + this.getClass.getSimpleName + Console.RESET
}

// age 1 brown cards
case class WoodPlace() extends BrownCard {
    override val res = Resources(wood = 1)
}
case class ClayPlace() extends BrownCard {
    override val res = Resources(clay = 1)
}
case class OrePlace() extends BrownCard {
    override val res = Resources(ore = 1)
}
case class StonePlace() extends BrownCard {
    override val res = Resources(stone = 1)
}
case class Baumschule() extends BrownCard {
    override val goldCost = 1
    override val res = Resources.dynamic(wood = 1, clay = 1)
}
case class Ausgrabungsstätte() extends BrownCard {
    override val goldCost = 1
    override val res = Resources.dynamic(stone = 1, clay = 1)
}
case class Tongrube() extends BrownCard {
    override val goldCost = 1
    override val res = Resources.dynamic(clay = 1, ore = 1)
}
case class Forstwirtschaft() extends BrownCard {
    override val goldCost = 1
    override val res = Resources.dynamic(wood = 1, stone =1)
}
case class Waldhöhle() extends BrownCard {
    override val goldCost = 1
    override val res = Resources.dynamic(wood = 1, ore = 1)
}
case class Mine() extends BrownCard {
    override val goldCost = 1
    override val res = Resources.dynamic(stone = 1, ore = 1)
}

// age 2 brown cards
case class Sägewerk() extends BrownCard {
    override val goldCost = 1
    override val res = Resources(wood = 2)
}
case class Bildhauerei() extends BrownCard {
    override val goldCost = 1
    override val res = Resources(stone = 2)
}
case class Ziegelbrennerei() extends BrownCard {
    override val goldCost = 1
    override val res = Resources(clay = 2)
}
case class Giesserei() extends BrownCard {
    override val goldCost = 1
    override val res = Resources(ore = 2)
}

// age 1 + 2 grey cards
case class Press() extends GreyCard {
    override val res = Resources(papyrus = 1)
}
case class Weavery() extends GreyCard {
    override val res = Resources(cloth = 1)
}
case class Glassery() extends GreyCard {
    override val res = Resources(glass = 1)
}

// age 1 blue cards
case class Pfandhaus() extends BlueCard {
    override val value = 3
}
case class Bäder() extends BlueCard {
    override val resourceReq = Resources(stone = 1)
    override val value = 3
    override val chains = List(Aquädukt())
}
case class Altar() extends BlueCard {
    override val value = 2
    override val chains = List(Tempel())
}
case class Theatre() extends BlueCard {
    override val value = 2
    override val chains = List(Statue())
}

// age 2 blue cards
case class Aquädukt() extends BlueCard {
    override val value = 5
    override val resourceReq = Resources(stone = 3)
}
case class Tempel() extends BlueCard {
    override val value = 3
    override val resourceReq = Resources(wood = 1, clay = 1, glass = 1)
    override val chains = List(Pantheon())
}
case class Statue() extends BlueCard {
    override val value = 4
    override val resourceReq = Resources(ore = 2, wood = 1)
    override val chains = List(Gärten())
}
case class Gericht() extends BlueCard {
    override val value = 4
    override val resourceReq = Resources(clay = 2, cloth = 1)
}

// age 3 blue cards
case class Pantheon() extends BlueCard {
    override val value = 7
    override val resourceReq = Resources(clay = 2, ore = 1, glass = 1, papyrus = 1, cloth = 1)
}
case class Gärten() extends BlueCard {
    override val value = 5
    override val resourceReq = Resources(clay = 2, wood = 1)
}
case class Rathaus() extends BlueCard {
    override val value = 6
    override val resourceReq = Resources(stone = 2, ore = 1, glass = 1)
}
case class Palast() extends BlueCard {
    override val value = 8
    override val resourceReq = Resources(wood = 1, stone = 1, clay = 1, ore = 1, glass = 1, papyrus = 1, cloth = 1)
}
case class Senat() extends BlueCard {
    override val value = 6
    override val resourceReq = Resources(wood = 2, stone = 1, ore = 1)
}



// age 1 yellow cards
case class Tavern() extends YellowCard {
    override def benefit(p: PlayerState, g: GameState) = p addGold 5
}
case class KontorOst() extends YellowCard {
    override def benefit(p: PlayerState, g: GameState) = p.copy(tradeRight = (1, p.tradeRight._2))
}
case class KontorWest() extends YellowCard {
    override def benefit(p: PlayerState, g: GameState) = p.copy(tradeLeft = (1, p.tradeRight._2))
}
case class Market() extends YellowCard {
    override def benefit(p: PlayerState, g: GameState) = p.copy(
        tradeLeft = (p.tradeRight._1, 1),
        tradeRight = (p.tradeRight._1, 1)
    )
}

// age 2 yellow cards
case class Forum() extends YellowCard {
    override val resourceReq = Resources(clay = 2)
    override def benefit(p: PlayerState, g: GameState) = p addNoTradeResources Resources.dynamic(cloth = 1, glass = 1, papyrus = 1)
    override val chains = List(Hafen())
}
case class Karawanserei() extends YellowCard {
    override val resourceReq = Resources(wood = 2)
    override def benefit(p: PlayerState, g: GameState) = p addNoTradeResources Resources.dynamic(wood = 1, stone = 1, ore = 1, clay = 1)
    override val chains = List(Leuchtturm())
}
case class Weinberg() extends YellowCard {
    override def benefit(p: PlayerState, g: GameState) = p addGold( p countAll( _.isInstanceOf[BrownCard], g) )
}
case class Basar() extends YellowCard {
    override def benefit(p: PlayerState, g: GameState) = p addGold( 2 * p.countAll( _.isInstanceOf[GreyCard], g) )
}

// age 3 yellow cards

case class Hafen() extends YellowCard {
    override val resourceReq = Resources(wood = 1, ore = 1, cloth = 1)
    override def benefit(p: PlayerState, g: GameState) = p addGold p.count( _.isInstanceOf[BrownCard] )
    override def worth(p: PlayerState, g: GameState) = p.cards.count( _.isInstanceOf[BrownCard] )
}
case class Leuchtturm() extends YellowCard {
    override val resourceReq = Resources(stone = 1, glass = 1)
    override def benefit(p: PlayerState, g: GameState) = p addGold p.count( _.isInstanceOf[YellowCard] )
    override def worth(p: PlayerState, g: GameState) = p.cards.count( _.isInstanceOf[YellowCard] )
}
case class Handelskammer() extends YellowCard {
    override val resourceReq = Resources(clay = 2, papyrus = 1)
    override def benefit(p: PlayerState, g: GameState) = p addGold(2 * p.count( _.isInstanceOf[GreyCard] ) )
    override def worth(p: PlayerState, g: GameState) = 2 * p.count( _.isInstanceOf[GreyCard] )
}
case class Arena() extends YellowCard {
    override val resourceReq = Resources(stone = 2, ore = 1)
    override def benefit(p: PlayerState, g: GameState) = p addGold(3 * p.wonderStuffed.length)
    override def worth(p: PlayerState, g: GameState) = p.wonderStuffed.length
}


// age 1 red cards
case class Befestigungsanlage() extends RedCard {
    override val value = 1
    override val resourceReq = Resources(wood = 1)
}
case class Kaserne() extends RedCard {
    override val value = 1
    override val resourceReq = Resources(ore = 1)
}
case class Wachturm() extends RedCard {
    override val value = 1
    override val resourceReq = Resources(clay = 1)
}

// age 2 red cards
case class Mauern() extends RedCard {
    override val value = 2
    override val resourceReq = Resources(stone = 3)
    override val chains = List(Verteidigungsanlage())
}
case class Trainingsgelände() extends RedCard {
    override val value = 2
    override val resourceReq = Resources(ore = 2, wood = 1)
    override val chains = List(Zirkus())
}
case class Ställe() extends RedCard {
    override val value = 2
    override val resourceReq = Resources(clay = 1, wood = 1, ore = 1)
}
case class Schiessplatz() extends RedCard {
    override val value = 2
    override val resourceReq = Resources(wood = 2, ore = 1)
}

// age 3 red cards
case class Verteidigungsanlage() extends RedCard {
    override val value = 3
    override val resourceReq = Resources(stone = 1, ore = 3)
}
case class Zirkus() extends RedCard {
    override val value = 3
    override val resourceReq = Resources(stone = 3, ore = 1)
}
case class Waffenlager() extends RedCard {
    override val value = 3
    override val resourceReq = Resources(wood = 2, ore = 1, cloth = 1)
}
case class Belagerungsmaschinen() extends RedCard {
    override val value = 3
    override val resourceReq = Resources(clay = 3, wood = 1)
}



// age 1 green cards
case class Apothecary() extends GreenCard {
    override val resourceReq = Resources(cloth = 1)
    override val chains = List(Arzneiausgabe(), Ställe())
    override val value = (1,0,0)
}
case class Werkstatt() extends GreenCard {
    override val resourceReq = Resources(glass = 1)
    override val chains = List(Laboratorium(), Schiessplatz())
    override val value = (0,1,0)
}
case class Skriptorium() extends GreenCard {
    override val resourceReq = Resources(papyrus = 1)
    override val chains = List(Bibliothek(), Gericht())
    override val value = (0,0,1)
}

// age 2 green cards
case class Arzneiausgabe() extends GreenCard {
    override val resourceReq = Resources(ore = 2, glass = 1)
    override val value = (1,0,0)
    override val chains = List(Loge(), Arena())
}
case class Laboratorium() extends GreenCard {
    override val resourceReq = Resources(clay = 2, papyrus = 1)
    override val value = (0,1,0)
    override val chains = List(Belagerungsmaschinen(), Observatorium())
}
case class Bibliothek() extends GreenCard {
    override val resourceReq = Resources(stone = 2, cloth = 1)
    override val value = (0,0,1)
    override val chains = List(Senat(), Universität())
}
case class Schule() extends GreenCard {
    override val resourceReq = Resources(wood = 1, papyrus = 1)
    override val value = (0,0,1)
    override val chains = List(Akademie(), Studierzimmer())
}

// age 3 green cards
case class Loge() extends GreenCard {
    override val resourceReq = Resources(clay = 2, papyrus = 1, cloth = 1)
    override val value = (1,0,0)
}
case class Observatorium() extends GreenCard {
    override val resourceReq = Resources(ore = 2, glass = 1, cloth = 1)
    override val value = (0,1,0)
}
case class Universität() extends GreenCard {
    override val resourceReq = Resources(wood = 2, papyrus = 1, glass = 1)
    override val value = (0,0,1)
    override val chains = List(Senat())
}
case class Akademie() extends GreenCard {
    override val resourceReq = Resources(stone = 3, glass = 1)
    override val value = (1,0,0)
}
case class Studierzimmer() extends GreenCard {
    override val resourceReq = Resources(wood = 1, papyrus = 1, cloth = 1)
    override val value = (0,1,0)
}

// age 3 purple cards

case class GuildWorkers() extends PurpleCard {
    override val resourceReq = Resources(ore = 2, clay = 1, stone = 1, wood = 1)
    override def worth(p: PlayerState, g: GameState) = p countNeighbors( _.isInstanceOf[BrownCard], g)
}
case class GuildArtisans() extends PurpleCard {
    override val resourceReq = Resources(ore = 2, stone = 2)
    override def worth(p: PlayerState, g: GameState) = 2 * p.countNeighbors( _.isInstanceOf[GreyCard], g)
}
case class GuildTraders() extends PurpleCard {
    override val resourceReq = Resources(glass = 1, papyrus = 1, cloth = 1)
    override def worth(p: PlayerState, g: GameState) = p countNeighbors( _.isInstanceOf[YellowCard], g)
}
case class GuildPhilosophy() extends PurpleCard {
    override val resourceReq = Resources(clay = 3, papyrus = 1, cloth = 1)
    override def worth(p: PlayerState, g: GameState) = p countNeighbors( _.isInstanceOf[GreenCard], g)
}
case class GuildSpies() extends PurpleCard {
    override val resourceReq = Resources(clay = 3, glass = 1)
    override def worth(p: PlayerState, g: GameState) = p countNeighbors( _.isInstanceOf[RedCard], g)
}
case class GuildStrategists() extends PurpleCard {
    override val resourceReq = Resources(ore = 2, stone = 1, cloth = 1)
    override def worth(p: PlayerState, g: GameState) = ((p lefty g).redlost + (p righty g).redlost) abs
}
case class GuildReeder() extends PurpleCard {
    override val resourceReq = Resources(wood = 3, glass = 1, papyrus = 1)
    override def worth(p: PlayerState, g: GameState) = p count(x => x.isInstanceOf[BrownOrGreyCard] || x.isInstanceOf[PurpleCard])
}
case class GuildScientists() extends PurpleCard {
    override val resourceReq = Resources(wood = 2, ore = 2, papyrus = 1)
    override def benefit(p: PlayerState, g: GameState) = p.copy(scienceWildCard = p.scienceWildCard+1)
}
case class GuildOfficials() extends PurpleCard {
    override val resourceReq = Resources(wood = 3, stone = 1, cloth = 1)
    override def worth(p: PlayerState, g: GameState) = p countNeighbors( _.isInstanceOf[BlueCard], g)
}
case class GuildBuilders() extends PurpleCard {
    override val resourceReq = Resources(stone = 2, clay = 2, glass = 1)
    override def worth(p: PlayerState, g: GameState) = p.wonderStuffed.length + (p lefty g).wonderStuffed.length + (p righty g).wonderStuffed.length
}
