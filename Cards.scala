
// yes those include both resources and goods
abstract class Resource()
// resources
case class Wood() extends Resource
case class Stone() extends Resource
case class Clay() extends Resource
case class Ore() extends Resource
// goods
case class Glass() extends Resource
case class Papyrus() extends Resource
case class Cloth() extends Resource

// card types
abstract class Card() {
    val goldCost: Int = 0
    val resourceReq: List[Resource] = List()
    def benefit(s: PlayerState): PlayerState
}
abstract class BrownCard() extends Card {
    val res: Resource
    def benefit(s: PlayerState) = s.copy(resources = res :: s.resources)
}
abstract class GreyCard() extends Card {
    val res: Resource
    def benefit(s: PlayerState) = s.copy(resources = res :: s.resources)

}
abstract class RedCard() extends Card {
    val value: Int
    def benefit(s: PlayerState) = s.copy(shields = s.shields + value)
}
abstract class BlueCard() extends Card {
    val value: Int
    def benefit(s: PlayerState) = s.copy(vp = s.vp + value)
}
abstract class YellowCard() extends Card
abstract class PurpleCard() extends Card


// age 1 brown cards
case class WoodPlace() extends BrownCard {
    override val res = Wood()
}
case class ClayPlace() extends BrownCard {
    override val res = Clay()
}
case class OrePlace() extends BrownCard {
    override val res = Ore()
}
case class StonePlace() extends BrownCard {
    override val res = Stone()
}

// age 1 + 2 grey cards
case class Press() extends GreyCard {
    override val res = Papyrus()
}
case class Weavery() extends GreyCard {
    override val res = Cloth()
}
case class Glassery() extends GreyCard {
    override val res = Glass()
}

// age 1 blue cards
case class Pfandhaus() extends BlueCard {
    override val value = 3
}
case class Bäder() extends BlueCard {
    override val resourceReq = List(Stone())
    override val value = 3
}
case class Altar() extends BlueCard {
    override val value = 2
}
case class Theatre() extends BlueCard {
    override val value = 2
}

// age 1 yellow cards
case class Tavern() extends YellowCard {
    def benefit(s: PlayerState) = s.copy(gold = s.gold + 5)
}

// age 1 red cards
case class Befestigungsanlage() extends RedCard {
    override val value = 1
    override val resourceReq = List(Wood())
}
case class Kaserne() extends RedCard {
    override val value = 1
    override val resourceReq = List(Ore())
}
case class Wachturm() extends RedCard {
    override val value = 1
    override val resourceReq = List(Clay())
}


object Card {
    def newGameHands(players: Int) = (players match {
        case 3 => List(
            WoodPlace(), ClayPlace(), StonePlace(), OrePlace(),
            Press(), Weavery(), Glassery(),
            Pfandhaus(), Bäder(), Altar(), Theatre(),
            Tavern(),
            Befestigungsanlage(), Kaserne(), Wachturm()
        )
    }) grouped(players) map(Hand) toList

}
