
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

    def pickCards(state: GameState): GameState = {
        val (p, ps) = (state.players.head, state.players.tail)
        val picks = {

            // this block must return an Action, which wraps a PlayerOption
            // including all required decisions

            // all options available to the player (including unavailable ones!)
            val options =
                // you can always discard
                OptionDiscard() ::
                // build a wonder stage, possibly
                p.wonder.categorize(p, (p lefty state).resources, (p righty state).resources) ::
                // or pick a card
                p.hand.options(p, ps.last, ps.head)

            options.zipWithIndex foreach { case (option, i) =>
                println(s"$i $option")
            }

            // todo~
            val option = options(readInt)

            // map possible actions
            val actions: List[Action] = (option match {
                case x: CardAvailable => ActionPick(x)
                case x @ CardTrade(card, either, left, right) => {
                    println(either)
                    println(left)
                    println(right)
                    // ActionPickWithTrade(x, left, right)
                    ActionDiscard(x)
                }
                case x: CardUnavailable => ActionDiscard(x)
            }) :: Nil

            actions.zipWithIndex foreach { case (option, i) =>
                println(s"$i $option")
            }

            actions(readInt)

        } :: (ps map { _.pickAny })

        state.draft(picks)
    }

}
