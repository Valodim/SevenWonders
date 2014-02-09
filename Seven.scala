import scala.util.control._

object SevenCli extends App {

    val brown = "\033[38;5;136m"
    val yellow = "\033[38;5;185m"
    val red = "\033[38;5;166m"
    val green = "\033[38;5;112m"
    val purple = "\033[38;5;129m"
    val blue = "\033[38;5;33m"
    val grey = "\033[38;5;241m"

    // initial game state
    val wonders = GameState.newWonders(4)
    val wonderSides = interactiveWonderSide(wonders.head).get :: wonders.tail.map(_.chooseRandom)
    var g = GameState.newGame(wonderSides)
    var continue = true

    while(continue && g.age > 0) {
        println(s"Age ${g.age}, ${g.cardsLeft} cards left")
        readLine() match {
            case "p" => { println(g) }
            case "q" => { continue = false }
            case "n" => interactiveTurn(g) map { g = _ }
            case "v" => println(g.players map( _.totalvp(g)) mkString "\n")
            case _ => { println("Invalid command") }
        }
    }

    println("See ya!")

    def interactiveWonderSide(wonder: Wonder): Option[WonderSide] = {
        println(s"You got ${wonder}! Pick a side:")
        wonder.sides.zipWithIndex.foreach {
            case (s,i) => println(s"$i: $s")
        }
        Exception.catching(classOf[NumberFormatException]).opt {readInt} flatMap wonder.sides.lift
    }

    def interactiveTurn(g: GameState): Option[GameState] = {
        val (p, ps) = (g.players.head, g.players.tail)
        // pick an action
        interactiveAction(p, g) flatMap {
            action => {
                // draftEarly step, this is where most of the game happens
                val (playersPrime, newDiscards, lateops) = g.draftEarly(action :: (ps map { _.pickAny }))

                // some LateActions may need conversion to LateApplicableActions (mostly Halikarnassos here)
                val lateopsPrime: List[(Int, LateApplicableAction)] = lateops map ( _ match {
                    // special treatment for babylon
                    case (i, o: LateBabylon) => {
                        val p = playersPrime(i)
                        println("Babylon's free action with remaining card:")
                        val x = interactiveAction(p, g.copy(players = playersPrime))
                        if(x.isEmpty) {
                            println("Invalid choice!")
                            return None
                        }
                        (i, LateApplicableBabylon(x.get))
                    }
                    // special treatment for halikarnassos
                    case (i, o: LateHalikarnassos) => {
                        val p = playersPrime(i)
                        println("Halikarnassos' free pick from the discard pile:")
                        val x = interactiveCard(newDiscards.collect{
                            case x if ! p.cards.exists( _ == x) => CardHalikarnassos(x)
                        }).map(LateApplicableHalikarnassos)
                        // non-functional breakout
                        if(x.isEmpty) {
                            println("Invalid choice!")
                            return None
                        }
                        (i, x.get)
                    }
                    case (i, o: LateApplicableAction) => (i, o)
                })

                // draftLate step, deals with late effects such as gold gains
                // from trade, drafts cards and handles new ages
                Some(g.draftLate(playersPrime, newDiscards, lateopsPrime))
            }
        }
    }

    def interactiveAction(p: PlayerState, g: GameState): Option[Action] = {

        // this block must return an Action, which wraps a PlayerOption
        // including all required decisions

        val lefty = p lefty g
        val righty = p righty g

        // all options available to the player (including unavailable ones!)
        val cardOptions = p.hand.options(p, lefty, righty)
        // not too fond of the use of var here~
        var options =
            // you can always discard
            OptionDiscard() ::
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
            case x: OptionOlympia => interactiveCard(cardOptions) map ActionPickOlympia
            // discard? just pick any~
            case x: OptionDiscard => interactiveCard(cardOptions) map ActionDiscard
            // if the player picked an available card (chain or all resources available), just wrap it in an action
            case x: CardFree => Some(ActionPick(x))
            // if trade is needed, get a trade instance as well
            case t: CardTrade => interactiveTrade(p, g, t) flatMap ( _.tradeOffer(p, t) )
            // it's a wonder - but which card to stuff?
            case x: WonderAvailable => {
                // stuff it
                interactiveCard(cardOptions) flatMap { stuff =>
                    x match {
                        case x: WonderFree => Some(ActionWonder(x, stuff))
                        case t: WonderTrade => interactiveTrade(p, g, t) flatMap ( _.tradeOffer(p, t, stuff) )
                    }
                }
            }
            case _ => {
                println("Illegal move")
                None
            }
        }

    }

    def interactiveCard[T <: CardOption](cardOptions: List[T]): Option[T] = {
        // display cards that can be stuffed
        cardOptions.zipWithIndex foreach { case (option, i) =>
            println(s"$i $option")
        }

        Exception.catching(classOf[NumberFormatException]).opt {readInt} flatMap cardOptions.lift
    }

    def interactiveTrade(p: PlayerState, g: GameState, t: TradeOption): Option[Trade] = {
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
