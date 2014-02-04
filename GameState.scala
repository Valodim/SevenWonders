
// summary of a player's current state
case class PlayerState(
    // player's current hand
    val hand: Hand,
    // player's (played) cards
    val cards: List[Card] = List(),
    // fixed resources
    val resources: Resources = Resources(),
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

    // returns options (free, as upgrade, tradeable (with attributes: total, either, left, right), unavailable)
    def options(left: PlayerState, right: PlayerState): (List[Card], List[Card], List[(Card,Resources,Resources,Resources,Resources)], List[Card]) = {
        val x1 = hand.filterAvailable(resources)
        val x2 = hand.filterAsUpgrade(cards)
        val x3 = hand.filterWithTrade(resources, left.resources, right.resources)
        (x1, x2, x3, cards diff x1 diff x2 diff x3)
    }

    override def toString = {
        s"""
  Stats: $gold Gold, $vp VP, $shields Shields, $science Science
  Resources: $resources
  $hand
        """
    }

}

case class Hand(
    val cards: List[Card]
) {
    def without(card: Card) = copy(cards filter { _ != card })

    lazy val pickAny = cards.head
    lazy val length = cards.length
    lazy override val toString = "Hand: " + cards.toString

    def at(i: Int) = cards(i)

    def filterAvailable(res: Resources) = cards filter ( _.resourceReq - res isEmpty )
    def filterAsUpgrade(cards: List[Card]) = cards intersect ( cards map { _.chains } flatten )
    def filterWithTrade(res: Resources, left: Resources, right: Resources) = cards map {
        card => 
            val req = card.resourceReq - res
            val either = req & left & right
            (
                card, req, either,
                either - right,
                either - left
            )
    }

}

case class GameState(
    val age: Int = 1,
    val cardsLeft: Int = 7,
    val players: List[PlayerState]
) {
    // draft with given card picks
    def draft(picks: List[Card]): GameState = {
        // derive new playerstates after card picks
        val (playersPrime: List[PlayerState], drafts) = (players zip picks).map {
            case (player, pick) => player draft pick
        } unzip

        // draft hands to next player (right-hand)
        copy(cardsLeft = cardsLeft-1, players = (playersPrime zip (drafts.tail ::: List(drafts.head))).map {
            case (player, hand) => player.copy(hand = hand)
        })
    }

    // draft with "any" pick
    def nextRoundAny = draft(players map { _.pickAny })

    override def toString = {
        val pstr = players.zipWithIndex map { case (p, i) => s"Player $i:" + p.toString + "\n" }

s"""\n-- Seven State --
Age: $age, Card left: $cardsLeft
Players {
    $pstr
}
"""
    }

}

object GameState {
    def newGame(): GameState = {
        GameState(players = Card.newGameHands(3) map { PlayerState(_) })
    }
}
