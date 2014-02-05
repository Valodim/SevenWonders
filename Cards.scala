
// yes those include both resources and goods
case class Resources(
    wood: Int = 0, stone: Int = 0, clay: Int = 0, ore: Int = 0,
    glass: Int = 0, papyrus: Int = 0, cloth: Int = 0
) {

    def isEmpty = this forall( _ == 0 )

    def &(that: Resources): Resources = this zip that map { case (l, r) => l min r }
    def +(that: Resources): Resources = this zip that map { case (l, r) => l + r }
    def -(that: Resources): Resources = this zip that map { case (l, r) => (l - r) max 0 }

    override def toString() = s"Resources: $wood / $stone / $clay / $ore // $glass / $papyrus / $cloth"

}

object Resources {
    implicit def fromList(l: List[Int]): Resources = {
        val i = l.toIterator
        // not too happy with this ,but oh well
        Resources(i.next, i.next, i.next, i.next, i.next, i.next)
    }
    implicit def toList(r: Resources): List[Int] = List(r.wood, r.stone, r.clay, r.ore, r.glass, r.papyrus, r.cloth)
}

// card types
abstract class Card() {
    val goldCost: Int = 0
    val resourceReq: Resources = Resources()
    val chains: List[Card] = List()

    def benefit(s: PlayerState): PlayerState
}
abstract class BrownCard() extends Card {
    val res: Resources
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


object Card {
    def newAgeHands(players: Int, age: Int) = ((players, age) match {
        case (3,1) => List(
            WoodPlace(), ClayPlace(), StonePlace(), OrePlace(),
            Press(), Weavery(), Glassery(),
            Pfandhaus(), Bäder(), Altar(), Theatre(),
            Tavern(),
            Befestigungsanlage(), Kaserne(), Wachturm()
        )
        case (3,2) => List(
            WoodPlace(), ClayPlace(), StonePlace(), OrePlace(),
            Press(), Weavery(), Glassery(),
            Pfandhaus(), Bäder(), Altar(), Theatre(),
            Tavern(),
            Befestigungsanlage(), Kaserne(), Wachturm()
        )
    }) grouped(5) map(Hand) toList
}
