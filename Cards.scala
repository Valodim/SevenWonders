abstract class BrownCard() extends Card {
    val res: Resources = Resources()
    def benefit(s: PlayerState) = s.copy(resources = res + s.resources)
}
abstract class GreyCard() extends Card {
    val res: Resources
    def benefit(s: PlayerState) = s.copy(resources = res + s.resources)

}
abstract class RedCard() extends Card {
    val value: Int
    def benefit(s: PlayerState) = s.copy(shields = s.shields + value)
}
abstract class GreenCard() extends Card {
    val value: (Int,Int,Int)
    def benefit(s: PlayerState) = s.copy(
        science = (
            s.science._1 + value._1,
            s.science._2 + value._2,
            s.science._3 + value._3
        )
    )
}
abstract class BlueCard() extends Card {
    val value: Int
    def benefit(s: PlayerState) = s.copy(bluevp = s.bluevp + value)
}
abstract class YellowCard() extends Card
abstract class PurpleCard() extends Card


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
}
case class Statue() extends BlueCard {
    override val value = 4
    override val resourceReq = Resources(ore = 2, wood = 1)
}
case class Gericht() extends BlueCard {
    override val value = 4
    override val resourceReq = Resources(clay = 2, cloth = 1)
}

// age 1 yellow cards
case class Tavern() extends YellowCard {
    def benefit(s: PlayerState) = s.copy(gold = s.gold + 5)
}
case class KontorOst() extends YellowCard {
    def benefit(s: PlayerState) = s.copy(tradeRight = (1, s.tradeRight._2))
}
case class KontorWest() extends YellowCard {
    def benefit(s: PlayerState) = s.copy(tradeLeft = (1, s.tradeRight._2))
}
case class Market() extends YellowCard {
    def benefit(s: PlayerState) = s.copy(
        tradeLeft = (s.tradeRight._1, 1),
        tradeRight = (s.tradeRight._1, 1)
    )
}

// age 2 yellow cards
case class Forum() extends YellowCard {
    override val resourceReq = Resources(clay = 2)
    def benefit(s: PlayerState) = s.copy(
        noTradeResources = s.noTradeResources + Resources.dynamic(cloth = 1, glass = 1, papyrus = 1)
    )
}
case class Karawanserei() extends YellowCard {
    override val resourceReq = Resources(wood = 2)
    def benefit(s: PlayerState) = s.copy(
        noTradeResources = s.noTradeResources + Resources.dynamic(wood = 1, stone = 1, ore = 1, clay = 1)
    )
}
case class Weinberg() extends YellowCard {
    def benefit(s: PlayerState) = s
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
}
case class Ställe() extends RedCard {
    override val value = 2
    override val resourceReq = Resources(clay = 1, wood = 1, ore = 1)
}
case class Schiessplatz() extends RedCard {
    override val value = 2
    override val resourceReq = Resources(wood = 2, ore = 1)
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
}
case class Laboratorium() extends GreenCard {
    override val resourceReq = Resources(clay = 2, papyrus = 1)
    override val value = (0,1,0)
}
case class Bibliothek() extends GreenCard {
    override val resourceReq = Resources(stone = 2, cloth = 1)
    override val value = (0,0,1)
}
case class Schule() extends GreenCard {
    override val resourceReq = Resources(wood = 1, papyrus = 1)
    override val value = (0,0,1)
}

