
object Seven extends Application {

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
            while(i < 0 || i > p.hand.length) i = readInt()

            p.hand.at(i)
        } :: (ps map { _.pickAny })

        state.draft(picks)
    }

}
