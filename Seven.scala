
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

            val options = p.hand.options(p, ps.last, ps.head)
            options.zipWithIndex foreach { case (option, i) =>
                println(s"$i $option")
            }

            // ugly :(
            var i = readInt
            options(i) match {
                case x: CardAvailable => ActionPick(x)
                case x @ CardTrade(card, either, left, right) => {
                    println(either)
                    println(left)
                    println(right)
                    ActionPickWithTrade(x, left, right)
                }
                case x: CardUnavailable => ActionDiscard(x)
            }

        } :: (ps map { _.pickAny })

        state.draft(picks)
    }

}
