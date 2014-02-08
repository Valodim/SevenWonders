import PlayerState.PlayerNumber

object PlayerState {
    type PlayerNumber = Int
}

// summary of a player's current state
case class PlayerState(
    // player's current hand
    val hand: Hand,
    // so many wonders~
    val wonder: Wonder,
    // player number
    val number: PlayerNumber,
    // fixed resources (starts with wonder resources)
    val resources: Resources,
    // fixed, untradable resources
    val noTradeResources: Resources = Resources(),
    // current wonder stage
    val wonderStuffed: List[Card] = List(),
    // player's (played) cards
    val cards: List[Card] = List(),
    // military power
    val shields: Int = 0,
    // static vp for military wins
    val redvp: Int = 0,
    // static negative vp for military losses
    val redlost: Int = 0,
    // moneys
    val gold: Int = 0,
    // trading, resources vs goods
    val tradeLeft: (Int, Int) = (2, 2),
    val tradeRight: (Int, Int) = (2, 2),
    // science, bitch! wheel, circledrawthingie, tablet
    val science: (Int, Int, Int) = (0, 0, 0)
) {

    // all resources (available to the player)
    lazy val allResources = resources + noTradeResources

    def play(card: Card, g: GameState) = card.benefit(card.pay(copy(hand without card, cards = card :: cards)), g)
    def playForFree(card: Card, g: GameState) = card.benefit(copy(hand without card),g)
    def discard(card: Card) = copy(hand without card)

    def addGold(amount: Int) = copy(gold = gold+amount)
    def addNoTradeResources(r: Resources) = copy(noTradeResources = noTradeResources+r)
    def addResources(r: Resources) = copy(resources = resources+r)

    lazy val pickAny = hand.pickAny.categorize(this, Resources(), Resources()) match {
        case card: CardAvailable => ActionPick(card)
        case card => ActionDiscard(card)
    }

    def lefty(g: GameState) = g.players( (number-1+g.players.length) % g.players.length)
    def righty(g: GameState) = g.players( (number+1) % g.players.length)
    def count(pred: (Card => Boolean)): Int = cards.count(pred)
    def countNeighbors(pred: (Card => Boolean), g: GameState): Int = lefty(g).count(pred) + righty(g).count(pred)
    def countAll(pred: (Card => Boolean), g: GameState): Int = count(pred) + countNeighbors(pred, g)

    override def toString = {
        s""" $wonder
  Stats: $gold Gold, $shields Shields, $redvp red VP, $science Science
  Cards: ${cards.mkString(",")}
  $resources
  $hand"""
    }

    def battle(g: GameState): PlayerState = {
        val vp = g.age match { case 1 => 1; case 2 => 3; case 3 => 5 }
        val (left, right) = (lefty(g), righty(g))
        // dat syntax :|
        val wins = (if(left.shields < shields) 1 else 0) + (if(right.shields < shields) 1 else 0)
        val losses = (if(left.shields > shields) 1 else 0) + (if(right.shields > shields) 1 else 0)
        copy(redvp = redvp + vp*wins, redlost = redlost-losses)
    }

    def totalvp(g: GameState): Int = {
        // cards' worth
        + cards.map{ _.worth(this, g) }.sum
        // wonder stages' worth
        + (wonder.stages take wonderStuffed.length).map{ _.worth(this, g) }.sum
        // battle wins and losses
        + redvp + redlost
        // consolidation prizes
        + gold/3
        // science!
        + {
            // still looking for a nicer way to do this
            val l = List(science._1, science._2, science._3)
            // squares + 7*min
            l.map( x => x * x ).sum + 7 * (l min)
        }
    }

}

case class Hand(
    val cards: List[Card]
) {
    def without(card: Card) = copy(cards filter { _ != card })

    lazy val pickAny = cards.head
    lazy val length = cards.length
    lazy override val toString = "Hand: " + cards.mkString(", ")

    def at(i: Int) = cards(i)

    // returns options (free, as upgrade, tradeable (with attributes: total, either, left, right), unavailable)
    def options(p: PlayerState, left: PlayerState, right: PlayerState): List[CardOption] = {
        cards map( _.categorize(p, left.resources, right.resources) )
    }


}

case class GameState(
    val age: Int = 1,
    val cardsLeft: Int = 7,
    val players: List[PlayerState],
    val discardPile: List[Card] = List()
) {
    // draft with given card picks
    def draft(actions: Seq[Action]): GameState = {
        // derive new playerstates after card picks
        val (playersPrim, discards, lateops) = (players zip actions).map {
            case (player, action) => action(player, this)
        } unzip3

        val playersPrime = playersPrim map { p =>
            var pPrime = p
            // ugly~ if you have a nicer way to do this, I'm all ears
            // only other way I can think of is transforming Action.apply and
            // reducing from there, but that's not really much better...
            lateops.flatten collect { case (p.number, o) => o } foreach { o =>
                // apply action!
                pPrime = o(pPrime, this)._1
            }
            pPrime
        }

        // draft to the right
        lazy val afterDraftLeft = (playersPrime zip (playersPrime.tail ::: List(playersPrime.head))).map {
            case (player, next) => player.copy(hand = next.hand)
        }
        // draft to the left
        lazy val afterDraftRight = (playersPrime zip (playersPrime.last :: playersPrime)).map {
            case (player, next) => player.copy(hand = next.hand)
        }

        // TODO new age (no drafts but new cards)

        copy(
            cardsLeft = cardsLeft-1,
            players = if(age % 2 == 0) afterDraftLeft else afterDraftRight,
            discardPile = discardPile ::: discards.collect { case Some(x) => x }
        )
    }

    def newage(): GameState = {
        val newCards = Card.newAgeHands(players.length, age+1)
        val newPlayers = (players.tail ::: List(players.head), players, players.head :: players.tail).zipped map {
            case (left, player, right) => player battle(this)
        }

        GameState(age+1, 7, newPlayers)
    }

    override def toString = {
        val pstr = players.zipWithIndex map { case (p, i) => s"Player ${i+1}:" + p.toString + "\n" } mkString

s"""\n-- Seven State --
Age: $age, Card left: $cardsLeft
Players {
$pstr
}"""
    }

}

object GameState {
    def newGame(): GameState = {

        val wonders = Wonder.newGameWonders.take(3)
        val hands = Card.newAgeHands(3, 1)

        GameState(players = (wonders zip hands).zipWithIndex map { case ((wonder, hand), i) => PlayerState(hand, wonder, i, wonder.res, gold = 3) })

    }
}

abstract class PlayerOption
case class OptionDiscard {
    override def toString() = s"${Console.RED}~${Console.RESET} Discard a card"
}
