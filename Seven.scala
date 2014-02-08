import scala.util.control._

object Seven extends App {

    // initial game state
    var g = GameState.newGame
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

    def interactiveTurn(g: GameState): Option[GameState] = {
        val (p, ps) = (g.players.head, g.players.tail)
        interactiveAction(p, g) flatMap {
            action => Some(g.draft(action :: (ps map { _.pickAny })))
        }
    }

    def interactiveAction(p: PlayerState, g: GameState): Option[Action] = {

        // this block must return an Action, which wraps a PlayerOption
        // including all required decisions

        val lefty = p lefty g
        val righty = p righty g

        // all options available to the player (including unavailable ones!)
        val cardOptions = p.hand.options(p, lefty, righty)
        val options =
            // you can always discard
            OptionDiscard() ::
            // build a wonder stage, possibly
            p.wonder.categorize(p, lefty.resources, righty.resources) ::
            // or any card option
            cardOptions

        options.zipWithIndex foreach { case (option, i) =>
            println(s"$i $option")
        }

        // read an int and look up value in options. None on numberformat or out of bounds
        val option: Option[PlayerOption] = Exception.catching(classOf[NumberFormatException]).opt {readInt} flatMap options.lift

        // further decisions depending on option picked (or None)
        option flatMap {
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

    def interactiveCard(cardOptions: List[CardOption]): Option[CardOption] = {
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
