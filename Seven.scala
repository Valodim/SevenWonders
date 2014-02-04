
object Seven extends Application {

    // initial game state
    var s = GameState.newGame

    while(true) {
        readLine() match {
            case "p" => { println(s) }
            case "n" => { s = pickCards(s) }
        }
    }


    def pickCards(state: GameState): GameState = {
        val (p, ps) = (state.players.head, state.players.tail)
        val picks = {
            println(p.hand)

            // ugly :(
            var i = readInt
            while(i < 0 || i > p.hand.length) i = readInt()

            p.hand.at(i)
        } :: (ps map { _.pickAny })

        state.draft(picks)
    }

}
