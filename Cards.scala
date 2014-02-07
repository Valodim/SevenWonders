// card types
abstract class Card() {
    val goldCost: Int = 0
    val resourceReq: Resources = Resources()
    val chains: List[Card] = List()

    def benefit(s: PlayerState): PlayerState

    def categorize(res: Resources, chains: List[Card], left: Resources, right: Resources): CardOption = {
        // can we chain-build this?
        if ( chains.map( _.chains ).flatten contains this )
            return CardChain(this)

        // calculate resources we don't have
        val req = resourceReq - res
        // if we have everything - great
        if(req isEmpty)
            return CardFree(this)

        // check if the required resources minus the potentially available ones is empty
        if( (req - left - right) isEmpty) {
            val either = req & left & right
            return CardTrade(this, either, either & right, either & left)
        }

        // otherwise - can't touch this
        CardUnavailable(this)
    }
}
abstract class BrownCard() extends Card {
    val res: Resources
    val resdyn: Option[Resources] = None
    def benefit(s: PlayerState) = s.copy(
        resources = res + s.resources,
        resdynamic = if(resdyn.isEmpty) s.resdynamic else resdyn.get :: s.resdynamic
    )
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
case class Tongrube() extends BrownCard {
    override val goldCost = 1
    override val res = Resources(clay = 1, ore = 1)
}
case class Forstwirtschaft() extends BrownCard {
    override val goldCost = 1
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

// age 1 green cards
case class Apothecary() extends GreenCard {
    override val value = (1,0,0)
}
case class Werkstatt() extends GreenCard {
    override val value = (0,1,0)
}
case class Skriptorium() extends GreenCard {
    override val value = (0,0,1)
}


object Card {
    def newAgeHands(players: Int, age: Int) = ((players, age) match {
        case (3,1) => List(
            WoodPlace(), ClayPlace(), StonePlace(), OrePlace(),
            Tongrube(), Forstwirtschaft(),
            Press(), Weavery(), Glassery(),
            Bäder(), Altar(), Theatre(),
            Befestigungsanlage(), Kaserne(), Wachturm(),
            Apothecary(), Werkstatt(), Skriptorium()
        )
        case (4,1) => List(
            WoodPlace(), ClayPlace(), StonePlace(), OrePlace(),
            Tongrube(), Forstwirtschaft(),
            Press(), Weavery(), Glassery(),
            Pfandhaus(), Bäder(), Altar(), Theatre(),
            Befestigungsanlage(), Kaserne(), Wachturm(),
            Apothecary(), Werkstatt(), Skriptorium()
        )
        case (3,2) => List(
            WoodPlace(), ClayPlace(), StonePlace(), OrePlace(),
            Press(), Weavery(), Glassery(),
            Pfandhaus(), Bäder(), Altar(), Theatre(),
            Tavern(),
            Befestigungsanlage(), Kaserne(), Wachturm()
        )
    }) grouped(7) map(Hand) toList
}

abstract class CardOption
case class CardFree(card: Card) extends CardOption {
    override def toString() = Console.GREEN + "+ " + Console.RESET + card
}

case class CardChain(card: Card) extends CardOption {
    override def toString() = Console.BLUE + "+ " + Console.RESET + card
}

case class CardTrade(card: Card, either: Resources, left: Resources, right: Resources) extends CardOption {
    override def toString() = Console.YELLOW + "+ " + Console.RESET + card
}

case class CardUnavailable(card: Card) extends CardOption {
    override def toString() = Console.RED + "— " + Console.RESET + card
}
