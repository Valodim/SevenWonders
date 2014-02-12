import scala.util.control._

/* (More or less) simple CLI reference UI for the engine.
 *
 * Though output is in parts scattered among the game mechanics, input comes
 * exclusively from inside this class. It should be possible to implement
 * different interfaces replacing just this object.
 */
object SevenCli extends App {

    val brown = "\033[38;5;136m"
    val yellow = "\033[38;5;185m"
    val red = "\033[38;5;166m"
    val green = "\033[38;5;112m"
    val purple = "\033[38;5;129m"
    val blue = "\033[38;5;33m"
    val grey = "\033[38;5;241m"

    val numPlayers = 4

    // all player interfaces. first player is interactive cli, others some sort of AIs
    val interfaces: List[Interface] = InterfaceCli :: (1 to numPlayers map { _ => AI.random } toList)

    // initial game state
    val wonders = GameState.newWonders(numPlayers)
    val wonderSides = (wonders zip interfaces) map { case (w, i) => i.chooseWonderSide(w).get }

    // GameState, updated continuously while the game is running
    var g = GameState.newGame(wonderSides)
    var continue = true

    while(continue && g.age > 0) {
        println(s"Age ${g.age}, ${g.cardsLeft} cards left")
        readLine() match {
            case "p" => { println(g) }
            case "q" => { continue = false }
            case "n" => turn(g) map { g = _ }
            case "v" => println(g.players map( _.totalvp(g)) mkString "\n")
            case _ => { println("Invalid command") }
        }
    }

    println("See ya!")

    // Wraps an entire turn, yielding a new GameState or None if there was any
    // kind of input error
    def turn(g: GameState): Option[GameState] = {

        val actions = (g.players zip interfaces) map {
            case (p, i) => i.chooseAction(p, g)
        }
        if(actions exists ( _.isEmpty ) ) {
            println("Illegal move")
            return None
        }

        // draftEarly step, this is where most of the game happens
        val (playersPrime, newDiscards, lateops) = g.draftEarly(actions map ( _.get ))

        // some LateActions may need conversion to LateApplicableActions (mostly Halikarnassos here)
        val lateopsPrime: List[(Int, LateApplicableAction)] = lateops map ( _ match {
            // special treatment for babylon
            case (i, LateBabylon) => {
                val choice = interfaces(i).specialBabylon(playersPrime(i), g.copy(players = playersPrime))
                (i, choice.get)
            }
            // special treatment for halikarnassos
            case (i, LateHalikarnassos) => {
                val p = playersPrime(i)
                val choice = interfaces(i).specialHalikarnassos(p, g.copy(players = playersPrime), newDiscards)
                (i, choice.get)
            }
            case (i, o: LateApplicableAction) => (i, o)
        })

        // draftLate step, deals with late effects such as gold gains
        // from trade, drafts cards and handles new ages
        Some(g.draftLate(playersPrime, newDiscards, lateopsPrime))
    }
}

object InterfaceCli extends Interface {

    // Yields a WonderSide for a given Wonder, or None on bad input
    def chooseWonderSide(wonder: Wonder): Option[WonderSide] = {
        println(s"You got ${wonder}! Pick a side:")
        wonder.sides.zipWithIndex.foreach {
            case (s,i) => println(s"$i: $s")
        }
        Exception.catching(classOf[NumberFormatException]).opt {readInt} flatMap wonder.sides.lift
    }


    // Yields an Action for a given player in a given GameState, or None on bad input
    def chooseAction(p: PlayerState, g: GameState): Option[Action] = {

        // this block must return an Action, which wraps a PlayerOption
        // including all required decisions

        val lefty = p lefty g
        val righty = p righty g

        // all options available to the player (including unavailable ones!)
        val cardOptions = p.hand.options(p, lefty, righty)
        // not too fond of the use of var here~
        var options =
            // you can always discard
            OptionDiscard ::
            // build a wonder stage, possibly
            p.wonder.categorize(p, lefty.resources, righty.resources) ::
            // or any card option
            cardOptions

        // can we use olympia's special?
        if(p.wonder.isInstanceOf[OlympiaA] && p.wonderStuffed.length >= 2 && p.wonder.asInstanceOf[OlympiaA].specialLastUsed < g.age)
            options = OptionOlympia() :: options

        options.zipWithIndex foreach { case (option, i) =>
            println(s"$i $option")
        }

        // read an int and look up value in options. None on numberformat or out of bounds
        val option: Option[PlayerOption] = Exception.catching(classOf[NumberFormatException]).opt {readInt} flatMap options.lift

        // further decisions depending on option picked (or None)
        option flatMap {
            // Olympia special!
            case x: OptionOlympia => chooseCard(cardOptions) map ActionPickOlympia
            // discard? just pick any~
            case OptionDiscard => chooseCard(cardOptions) map(ActionDiscard(_))
            // if the player picked an available card (chain or all resources available), just wrap it in an action
            case x: CardFree => Some(ActionPick(x))
            // if trade is needed, get a trade instance as well
            case t: CardTrade => decideTrade(p, g, t) flatMap ( _.tradeOffer(p, t) )
            // it's a wonder - but which card to stuff?
            case x: WonderAvailable => {
                // stuff it
                chooseCard(cardOptions) flatMap { stuff =>
                    x match {
                        case x: WonderFree => Some(ActionWonder(x, stuff))
                        case t: WonderTrade => decideTrade(p, g, t) flatMap ( _.tradeOffer(p, t, stuff) )
                    }
                }
            }
            case _ => {
                println("Illegal move")
                None
            }
        }

    }

    def specialBabylon(p: PlayerState, g: GameState): Option[LateApplicableBabylon] = {
        println("Babylon's free action with remaining card:")
        val x = chooseAction(p, g)
        if(x.isEmpty) println("Invalid choice!")
        x map(LateApplicableBabylon)
    }

    def specialHalikarnassos(p: PlayerState, g: GameState, newDiscards: List[Card]): Option[LateApplicableHalikarnassos] = {
        println("Halikarnassos' free pick from the discard pile:")
        val x = chooseCard(newDiscards.collect{
            case x if ! p.cards.exists( _ == x) => CardHalikarnassos(x)
        })
        // non-functional breakout
        if(x.isEmpty) println("Invalid choice!")
        x map(LateApplicableHalikarnassos)
    }

    // Yields a CardOption, or None on bad input
    def chooseCard[T <: CardOption](cardOptions: List[T]): Option[T] = {
        // display cards that can be stuffed
        cardOptions.zipWithIndex foreach { case (option, i) =>
            println(s"$i $option")
        }

        Exception.catching(classOf[NumberFormatException]).opt {readInt} flatMap cardOptions.lift
    }

    // Yields a Trade for a TradeOption, or None on bad input
    def decideTrade(p: PlayerState, g: GameState, t: TradeOption): Option[Trade] = {
        val (fixedLeft, fixedRight) = t.fixedSums(p)
        println(s"left: $fixedLeft, ${t.left}\nright: $fixedRight, ${t.right}\neither: ${t.either}\n")
        if(t.either.isEmpty)
            Some(Trade(fixedLeft, fixedRight))
        else {
            val (extraLeft: List[Int], extraRight: List[Int]) = (t.eitherSplit zip t.splitCosts(p)).map{ case (res, (l,r)) =>
                println(s"Buy $res l($l) / r($r)")
                readLine match {
                    case "l" => (l,0)
                    case "r" => (0,r)
                    case _ => return None
                }
            }.unzip
            Some(Trade(fixedLeft + extraLeft.sum, fixedRight + extraRight.sum))
        }
    }

}
