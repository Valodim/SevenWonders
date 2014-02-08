import scala.util.Random

// card types
abstract class Card() {
    val goldCost: Int = 0
    val resourceReq: Resources = Resources()
    val chains: List[Card] = List()

    def pay(p: PlayerState): PlayerState = p.copy(gold = p.gold-goldCost)
    def benefit(p: PlayerState): PlayerState

    /** Central method for categorizing if and how a player can play a card.
     * This is the main place where resource, gold and other requirements for
     * cards are checked.
     *
     * @returns A CardOption instance, which is required to instantiate a
     * PickAction object, which is the only way a player can play cards.
     */
    def categorize(p: PlayerState, left: Resources, right: Resources): CardOption = {
        // we cannot ever play duplicate cards!
        if ( p.cards contains this )
            return CardDuplicate(this)

        // can we chain-build this? no resource required in that case
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
        if( (req - (left + right)) isEmpty) {
            val leftOnly = req - right
            val rightOnly = req - left
            // TODO the "either" isn't quite right here
            return CardTrade(this, req - leftOnly - rightOnly, leftOnly, rightOnly)
        }

        // otherwise - can't touch this
        CardUnavailable(this)
    }
}

object Card {
    def newAgeHands(players: Int, age: Int) = ((players, age) match {
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
        case (3,3) => List(
            Pantheon(), Gärten(), Rathaus(), Palast(), Senat(),
            Hafen(), Leuchtturm(), Arena(),
            Verteidigungsanlage(), Waffenlager(), Belagerungsmaschinen(),
            Loge(), Observatorium(), Universität(), Akademie(), Studierzimmer()
        )
    }) ::: (
        if(age == 3) {
            List(
                GuildWorkers(), GuildArtisans(), GuildTraders(), GuildPhilosophy(),
                GuildSpies(), GuildStrategists(), GuildReeder(), GuildScientists(),
                GuildOfficials(), GuildBuilders()
            ) take (players+2)
        } else
            Nil
    ) grouped(7) map(Hand) toList
}

abstract class CardOption {
    val card: Card
}
abstract class CardAvailable extends CardOption
case class CardFree(card: Card) extends CardAvailable {
    override def toString() = Console.GREEN + "+ " + Console.RESET + card
}

case class CardChain(card: Card) extends CardAvailable {
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
