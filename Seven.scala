
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
            val (oFree, oChain, oTrade, oUnav) = p.options(ps.last, ps.head)
            if(oFree.length > 0)
                println("free: " + oFree.mkString(","))
            if(oChain.length > 0)
                println("chain: " + oChain.mkString(","))
            if(oTrade.length > 0)
                println("trade: " + oTrade.mkString(","))
            if(oUnav.length > 0)
                println("unavailable: " + oUnav.mkString(","))

            // ugly :(
            var i = readInt
            while(i < 0 || i > p.hand.length) i = readInt()

            p.hand.at(i)
        } :: (ps map { _.pickAny })

        state.draft(picks)
    }

}
