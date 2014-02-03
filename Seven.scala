object HelloWorld extends Application {
    var s = GameState.newGame
    println(s)
    s = s.nextRoundAny
    println(s)
}

// summary of a player's current state
case class PlayerState(
    // player's current hand
    val hand: Hand,
    // player's cards
    val cards: List[Card] = List(),
    // fixed resources
    val resources: List[Resource] = List(),
    // military power
    val shields: Int = 0,
    // static victory points
    val vp: Int = 0,
    // moneys
    val gold: Int = 0,
    // science, bitch! wheel, tablet, circledrawthingie
    val science: (Int, Int, Int) = (0, 0, 0)
) {

    def +(card: Card) = card benefit copy(cards = card :: cards)

    def draft(card: Card) = (this + card, hand without card)
    lazy val pickAny = hand.pickAny
}

case class Hand(
    val cards: List[Card]
) {
    def without(card: Card) = copy(cards filter { _ != card })
    lazy val pickAny = cards.head
}

case class GameState(
    val players: List[PlayerState]
) {
    // draft with given card picks
    def draft(picks: List[Card]): GameState = {
        // derive new playerstates after card picks
        val (playersPrime: List[PlayerState], drafts) = (players zip picks).map {
            case (player, pick) => player draft pick
        } unzip

        // draft hands to next player (right-hand)
        copy(players = (playersPrime zip (drafts.tail ::: List(drafts.head))).map {
            case (player, hand) => player.copy(hand = hand)
        })
    }

    // draft with "any" pick
    def nextRoundAny = draft(players map { _.pickAny })
}

object GameState {
    def newGame(): GameState = {
        GameState(Card.newGameHands(3) map { PlayerState(_) })
    }
}
