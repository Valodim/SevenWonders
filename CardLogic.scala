import scala.util.Random

// card types
abstract class Card() {
    // basic card attributes
    val goldCost: Int = 0
    val resourceReq: Resources = Resources()
    val chains: List[Card] = List()

    // only includes gold, no check is done here!
    def pay(p: PlayerState): PlayerState = p.copy(gold = p.gold-goldCost)
    // add instant benfits to player state (excluding vp worth!)
    def benefit(p: PlayerState, g: GameState): PlayerState = p
    // worth in vp
    def worth(p: PlayerState, g: GameState): Int = 0

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
            return CardJustFree(this)

        // check if the required resources minus the potentially available ones is empty
        if( (req - (left + right)) isEmpty) {
            val leftOnly = req - right
            val rightOnly = req - left

            // see if a trade option is viable
            val trade = CardTrade(this, req - leftOnly - rightOnly, leftOnly, rightOnly)

            // if it is, make the offer
            return if(trade.minCosts(p) + goldCost <= p.gold)
                trade
            else
                // otherwise, too bad~
                CardTradeInsufficientFunds(this, req - leftOnly - rightOnly, leftOnly, rightOnly)
        }

        // otherwise - can't touch this
        CardUnavailable(this)
    }

    override def toString() = this.getClass.getSimpleName

}

object Card {
    def newAgeHands(players: Int, age: Int) = Random.shuffle( (age match {
        case 1 => List(
            List( // 3 players
                WoodPlace(), ClayPlace(), StonePlace(), OrePlace(), Tongrube(), Forstwirtschaft(),
                Press(), Weavery(), Glassery(),
                Bäder(), Altar(), Theatre(),
                KontorOst(), KontorWest(), Market(),
                Befestigungsanlage(), Kaserne(), Wachturm(),
                Apothecary(), Werkstatt(), Skriptorium()
            ),
            List( // 4 players
                WoodPlace(), OrePlace(), Ausgrabungsstätte(),
                Pfandhaus(), Tavern(),
                Wachturm(),
                Skriptorium()
            ),
            List( // 5 players
                StonePlace(), ClayPlace(), Waldhöhle(),
                Altar(),
                Tavern(),
                Kaserne(),
                Apothecary()
            ),
            List( // 6 players
                Baumschule(), Mine(),
                Press(), Weavery(), Glassery(),
                Theatre(), Market()
            ),
            List( // 7 players
                Bäder(), Pfandhaus(),
                Tavern(), KontorOst(), KontorWest(),
                Befestigungsanlage(), Werkstatt()
            )
        )
        case 2 => List(
            List( // 3 players
                Sägewerk(), Bildhauerei(), Ziegelbrennerei(), Giesserei(),
                Press(), Weavery(), Glassery(),
                Aquädukt(), Tempel(), Statue(), Gericht(),
                Forum(), Karawanserei(), Weinberg(),
                Mauern(), Ställe(), Schiessplatz(),
                Arzneiausgabe(), Laboratorium(), Bibliothek(), Schule()
            ),
            List( // 4 players
                Sägewerk(), Bildhauerei(), Ziegelbrennerei(), Giesserei(),
                Basar(),
                Trainingsgelände(),
                Arzneiausgabe()
            ),
            List( // 5 players
                Press(), Weavery(), Glassery(),
                Karawanserei(),
                Gericht(),
                Ställe(),
                Laboratorium()
            ),
            List( // 6 players
                Tempel(),
                Forum(), Karawanserei(), Weinberg(),
                Trainingsgelände(), Schiessplatz(),
                Bibliothek()
            ),
            List( // 7 players
                Aquädukt(), Statue(),
                Forum(), Basar(),
                Mauern(), Trainingsgelände(),
                Schule()
            )
        )
        case 3 => List(
            List( // 3 players
                Pantheon(), Gärten(), Rathaus(), Palast(), Senat(),
                Hafen(), Leuchtturm(), Arena(),
                Verteidigungsanlage(), Waffenlager(), Belagerungsmaschinen(),
                Loge(), Observatorium(), Universität(), Akademie(), Studierzimmer()
            ),
            List( // 4 players
                Gärten(),
                Hafen(), Handelskammer(),
                Zirkus(), Waffenlager(),
                Universität()
            ),
            List( // 5 players
                Rathaus(), Senat(),
                Arena(),
                Zirkus(), Belagerungsmaschinen(),
                Studierzimmer()
            ),
            List( // 6 players
                Pantheon(), Rathaus(),
                Leuchtturm(), Handelskammer(),
                Zirkus(),
                Loge()
            ),
            List( // 7 players
                Palast(),
                Verteidigungsanlage(), Waffenlager(),
                Arena(),
                Observatorium(), Akademie()
            )
        )
    }).take(players-2).flatten ::: (
        if(age == 3) {
            List(
                GuildWorkers(), GuildArtisans(), GuildTraders(), GuildPhilosophy(),
                GuildSpies(), GuildStrategists(), GuildReeder(), GuildScientists(),
                GuildOfficials(), GuildBuilders()
            ) take (players+2)
        } else
            Nil
    )) grouped(7) map(Hand) toList
}

abstract class CardOption extends PlayerOption {
    val card: Card
}
abstract class CardAvailable extends CardOption
abstract class CardFree extends CardAvailable
case class CardJustFree(card: Card) extends CardFree {
    override def toString() = Console.GREEN + "+ " + Console.RESET + card
}

case class CardChain(card: Card) extends CardFree {
    override def toString() = Console.BLUE + "+ " + Console.RESET + card
}

case class CardTrade(card: Card, either: Resources, left: Resources, right: Resources) extends CardAvailable with TradeOption {
    override def toString() = s"${Console.YELLOW}+${Console.RESET} $card [${either.count}e/${left.count}l/${right.count}r]"
}
case class CardTradeInsufficientFunds(card: Card, either: Resources, left: Resources, right: Resources) extends CardOption with TradeOption {
    override def toString() = s"${Console.YELLOW}—${Console.RESET} $card [${either.count}e/${left.count}l/${right.count}r]"
}

case class CardInsufficientFunds(card: Card) extends CardOption {
    override def toString() = Console.RED + "— " + Console.RESET + card
}

case class CardDuplicate(card: Card) extends CardOption {
    override def toString() = Console.RED + "— " + Console.RESET + card
}

case class CardUnavailable(card: Card) extends CardOption {
    override def toString() = s"${Console.RED}—${Console.RESET} $card"
}
