
object HelloWorld extends Application {
    // initial game state
    var s = GameState.newGame

    while(true) {
        readLine() match {
            case "p" => { println(s) }
            case "n" => { s = s.nextRoundAny }
        }
    }

}

