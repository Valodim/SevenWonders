import scala.util.Random

// card types
abstract class Card() {
    val goldCost: Int = 0
    val resourceReq: Resources = Resources()
    val chains: List[Card] = List()

    def pay(p: PlayerState): PlayerState = p.copy(gold = p.gold-goldCost)
    def benefit(p: PlayerState): PlayerState

    def categorize(p: PlayerState, left: Resources, right: Resources): CardOption = {
        if ( p.cards contains this )
            return CardDuplicate(this)

        // can we chain-build this?
        if ( p.cards.map( _.chains ).flatten contains this )
            return CardChain(this)

        // Insufficient moneyz?
        if ( p.gold < goldCost )
            return CardInsufficientFunds(this)

        // calculate resources we don't have
        val req = resourceReq - p.allResources
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

object Card {
    def newAgeHands(players: Int, age: Int) = Random.shuffle((players, age) match {
        case (3,1) => List(
            WoodPlace(), ClayPlace(), StonePlace(), OrePlace(),
            Tongrube(), Forstwirtschaft(),
            Press(), Weavery(), Glassery(),
            Bäder(), Altar(), Theatre(),
            KontorOst(), KontorWest(), Market(),
            Befestigungsanlage(), Kaserne(), Wachturm(),
            Apothecary(), Werkstatt(), Skriptorium()
        )
        case (4,1) => List(
            WoodPlace(), WoodPlace(), ClayPlace(), StonePlace(), OrePlace(), OrePlace(),
            Tongrube(), Forstwirtschaft(), Ausgrabungsstätte(),
            Press(), Weavery(), Glassery(),
            Pfandhaus(), Bäder(), Altar(), Theatre(),
            Tavern(), KontorOst(), KontorWest(), Market(),
            Befestigungsanlage(), Kaserne(), Wachturm(), Wachturm(),
            Apothecary(), Werkstatt(), Skriptorium()
        )
        case (3,2) => List(
            Sägewerk(), Bildhauerei(), Ziegelbrennerei(), Giesserei(),
            Press(), Weavery(), Glassery(),
            Aquädukt(), Tempel(), Statue(), Gericht(),
            Forum(), Karawanserei(), Weinberg(),
            Mauern(), Ställe(), Schiessplatz(),
            Arzneiausgabe(), Laboratorium(), Bibliothek(), Schule()
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

case class CardInsufficientFunds(card: Card) extends CardOption {
    override def toString() = Console.RED + "— " + Console.RESET + card
}

case class CardDuplicate(card: Card) extends CardOption {
    override def toString() = Console.RED + "— " + Console.RESET + card
}

case class CardUnavailable(card: Card) extends CardOption {
    override def toString() = Console.RED + "— " + Console.RESET + card
}