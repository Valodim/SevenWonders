
object Seven extends App {

    // initial game state
    var s = GameState.newGame
    var continue = true

    while(continue) {
        readLine() match {
            case "p" => { println(s) }
            case "n" => { s = pickCards(s) }
            case "q" => { continue = false }
        }
    }

    def pickCards(g: GameState): GameState = {
        val (p, ps) = (g.players.head, g.players.tail)
        val picks = {
            interactiveAction(p, g)
        } :: (ps map { _.pickAny })
        g.draft(picks)
    }

    def interactiveAction(p: PlayerState, g: GameState): Action = {

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

        // todo~
        val option = options(readInt)

        // further decisions depending on option picked
        option match {
            // if the player picked an available card, just wrap it in an action
            case x: CardAvailable => ActionPick(x)
            // if trade is needed, get a trade instance as well
            case x @ CardTrade(card, either, left, right) => {
                // todo
                ActionPickWithTrade(x, TradeCard(x, 0, 0, 0))
            }
            // free wonder - but which card to stuff?
            case x: WonderFree => ActionWonder(x, cardOptions(0))
            case x @ WonderTrade(stage, either, left, right) => {
                // todo
                ActionWonderWithTrade(x, TradeWonder(x, 0, 0, 0), cardOptions(0))
            }
        }

    }

}
