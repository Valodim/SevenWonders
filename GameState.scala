import PlayerState.PlayerNumber

object PlayerState {
    type PlayerNumber = Int

    // used by the benefit function to allow easy empty LateAction lists
    implicit def toLateActionTuple(p: PlayerState): (PlayerState, List[(PlayerNumber, LateAction)]) = (p, Nil)
    // similar for Action.apply
    implicit def toDiscardLateActionTuple(p: PlayerState): (PlayerState, Option[Card], List[(PlayerNumber, LateAction)]) = (p, None, Nil)
}

/* The PlayerState is the center of all game logic. It contains all information
 * about a single player at one specific point in time. This includes his position
 * in relation to other players, his Wonder, Hand, Gold, played Cards, and a
 * couple of other relevant attributes for different game mechanisms.
 *
 * A change of player state always yields a new PlayerState instance, updating
 * the relevant attributes. Most significantly, this happens when the player
 * picks a card which is then added to the list of played cards, more often
 * than not yielding some sort of instant benefit which further alters the
 * state.
 *
 * There are also a number of convenience methods available for querying player
 * state, including neighbors. Note that information about the latter is only
 * indirectly available through a passed GameState.
 */
case class PlayerState(
    // player's current hand
    val hand: Hand,
    // so many wonders~
    val wonder: WonderSide,
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
    val science: (Int, Int, Int) = (0, 0, 0),
    val scienceWildCard: Int = 0
) {

    lazy val name = s"Player ${number+1}"

    // all resources (available to the player)
    lazy val allResources = resources + noTradeResources
    lazy val costsLeft = List.fill(4)(tradeLeft._1) ::: List.fill(3)(tradeLeft._2)
    lazy val costsRight = List.fill(4)(tradeRight._1) ::: List.fill(3)(tradeRight._2)

    def play(card: Card, g: GameState) = card.benefit(card.pay(copy(hand without card, cards = card :: cards)), g)
    def playForFree(card: Card, g: GameState) = card.benefit(copy(hand without card, cards = card :: cards), g)
    def discard(card: Card) = copy(hand without card)

    def addGold(amount: Int) = copy(gold = gold+amount)
    def addNoTradeResources(r: Resources) = copy(noTradeResources = noTradeResources+r)
    def addResources(r: Resources) = copy(resources = resources+r)

    def lefty(g: GameState) = g.players( (number-1+g.players.length) % g.players.length)
    def righty(g: GameState) = g.players( (number+1) % g.players.length)
    def count(pred: (Card => Boolean)) = cards.count(pred)
    def filter(pred: (Card => Boolean)) = cards.filter(pred)
    def countNeighbors(pred: (Card => Boolean), g: GameState): Int = lefty(g).count(pred) + righty(g).count(pred)
    def countAll(pred: (Card => Boolean), g: GameState): Int = count(pred) + countNeighbors(pred, g)

    override def toString = {
        s""" $wonder
  Stats: $gold Gold, $shields Shields, $redvp red VP, $redlost red -VP, $science + $scienceWildCard Science 
  Trade cost: l$tradeLeft r$tradeRight, ${wonderStuffed.length} cards stuffed
  Cards: ${cards.mkString(", ")}
  Private Resources: $noTradeResources
  Resources: $resources"""
    }

    def battle(g: GameState): PlayerState = {
        val vp = g.age match { case 1 => 1; case 2 => 3; case 3 => 5 }
        val (left, right) = (lefty(g), righty(g))
        // dat syntax :|
        val wins = (if(left.shields < shields) 1 else 0) + (if(right.shields < shields) 1 else 0)
        val losses = (if(left.shields > shields) 1 else 0) + (if(right.shields > shields) 1 else 0)
        println(s"Battle: $name gets $wins wins and $losses losses, for a total of ${vp*wins-losses} VP!")
        copy(redvp = redvp + vp*wins, redlost = redlost-losses)
    }

    def vp_red(g: GameState) = redvp + redlost
    def vp_coins(g: GameState) = gold/3
    def vp_wonder(g: GameState) = (wonder.stages take wonderStuffed.length).map{ _.worth(this, g) }.sum
    def vp_blue(g: GameState) = filter(_.isInstanceOf[BlueCard]).map{ _.worth(this, g) }.sum
    def vp_yellow(g: GameState) = filter(_.isInstanceOf[YellowCard]).map{ _.worth(this, g) }.sum
    def vp_purple(g: GameState) = filter(_.isInstanceOf[PurpleCard]).map{ _.worth(this, g) }.sum
    def vp_green(g: GameState): (Int,Int) = {

            // still looking for a nicer way to do this
            val l = List(science._1, science._2, science._3)

            def sciencevp(l: List[Int]) = l.map( x => x * x ).sum + 7 * (l min)

            val maxscience: List[Int] = scienceWildCard match {
                // a general case is surprisingly complicated. doesn't occur in
                // the game, but I'm not really happy with relying on special
                // cases like this~
                case 0 => l
                case 1 => List(1,0,0).permutations map( _ zip l map { case (x,y) => x+y }) maxBy sciencevp
                case 2 => (for {
                        x <- List(1,0,0).permutations
                        y <- List(1,0,0).permutations
                    } yield (x zip y zip l) map { case ((x1,x2),x3) => x1+x2+x3 }) maxBy(sciencevp)
            }
            if(scienceWildCard > 0) {
                println(s"Using $scienceWildCard science wildcards as (${maxscience.mkString(",")})")
            }

            // squares + 7*min
            val squares = maxscience.map( x => x * x ).sum
            val sets = (maxscience min)

            (squares,sets)
    }

    def totalvp(g: GameState): Int =  {
        val vp = List(vp_red _, vp_coins _, vp_wonder _, vp_blue _, vp_yellow _, vp_purple _) map (_(g)) sum
        val (squares, sets) = vp_green(g)
        vp + squares + sets
    }

    def describeTotalVP(g: GameState): String = {

        // science... always special
        val (squares, sets) = vp_green(g)
        // all other vp values
        val vp = List(vp_red _, vp_coins _, vp_wonder _, vp_blue _, vp_yellow _, vp_purple _).map (_(g))

        val colors = List(SevenCli.red, SevenCli.yellow, Console.RESET, SevenCli.blue, SevenCli.yellow, SevenCli.purple)
        val vpstrings = vp zip colors map {
            case (v, c) => c + v + Console.RESET
        } mkString("+")

        vpstrings + s"+${SevenCli.green}($squares+7*$sets)${Console.RESET}" + s" = ${vp.sum+squares+7*sets}"

    }

}

case class Hand(
    val cards: List[Card]
) {
    def without(card: Card) = copy(
        // Yeah you really gotta patch~ it out
        cards.patch( cards indexOf(card), Nil, 1)
    )

    lazy val length = cards.length
    lazy override val toString = "Hand: " + cards.mkString(", ")

    // returns options (free, as upgrade, tradeable (with attributes: total, either, left, right), unavailable)
    def options(p: PlayerState, left: PlayerState, right: PlayerState): List[CardOption] = {
        cards map( _.categorize(p, left.resources, right.resources) )
    }
}

/* The GameState class contains the entire state of the game, instances often
 * but not always representing a state after (and before) the players' turn.
 * Most information is contained in the list of PlayerStates.
 */
case class GameState(
    val age: Int = 1,
    val cardsLeft: Int = 7,
    val players: List[PlayerState],
    val discardPile: List[Card] = List()
) {
    // draft with given card picks
    def draftEarly(actions: Seq[Action]): (List[PlayerState], List[Card], List[(Int,LateAction)]) = {
        // derive new playerstates after card picks
        val (playersPrime1, discards, lateops) = (players zip actions).map {
            case (player, action) => {
                println(action.describe(player, this))
                action(player, this)
            }
        } unzip3

        // gonna need this for Halikarnassos at some point
        var newDiscards = discardPile ::: discards.collect { case Some(x) => x }
        var lateopsPrime = lateops.flatten

        // if this was the last pick, ...
        if(cardsLeft == 2) {
            // add remaining cards from all players' hands to the discard pile
            newDiscards = newDiscards ::: playersPrime1.map{ _.hand.cards }.flatten

            // bind endOfAge lateops
            lateopsPrime = lateopsPrime ::: playersPrime1.flatMap(
                p => p.wonder.stages.take(p.wonderStuffed.length).flatMap(_.endOfAgeBenefit(p))
            )
        }

        (playersPrime1, newDiscards, lateopsPrime)

    }

    def draftLate(playersPrime1: List[PlayerState], newDiscards: List[Card], lateops: List[(Int,LateApplicableAction)]): GameState = {

        val playersPrime2 = playersPrime1 map { p =>
            var pPrime = p
            // ugly~ if you have a nicer way to do this, I'm all ears
            // only other way I can think of is transforming Action.apply and
            // reducing from there, but that's not really much better...
            lateops collect { case (p.number, o) => o } foreach { o =>
                // describe action!
                println(o.describe(pPrime, this))
                // apply action!
                pPrime = o(pPrime, this, newDiscards)._1
            }
            pPrime
        }

        // draft to the right
        lazy val afterDraftLeft = (playersPrime2 zip (playersPrime2.tail ::: List(playersPrime2.head))).map {
            case (player, next) => player.copy(hand = next.hand)
        }
        // draft to the left
        lazy val afterDraftRight = (playersPrime2 zip (playersPrime2.last :: playersPrime2)).map {
            case (player, next) => player.copy(hand = next.hand)
        }

        // if this is a new age, handle this in another method
        if(cardsLeft == 2) {
            newAge(playersPrime2, newDiscards)
        // otherwise, just apply the new values
        } else
            copy(
                cardsLeft = cardsLeft-1,
                players = if(age % 2 == 0) afterDraftLeft else afterDraftRight,
                discardPile = newDiscards
            )

    }

    def newAge(playersPrime2: List[PlayerState], newDiscards: List[Card]): GameState = {
        // Do battle!
        val playersPrime3 = playersPrime2 map { _.battle(copy(players = playersPrime2)) }

        // this is it?
        if(age == 3)
            return endOfGame(playersPrime3, newDiscards)

        // Hand out a shiny new set of cards
        val playersPrime4 = playersPrime3 zip Card.newAgeHands(players.length, age+1) map {
            case (player, hand) => player.copy(hand)
        }

        // It's a brand new day!
        GameState(age+1, 7, playersPrime4, discardPile = newDiscards)
    }

    def endOfGame(playersPrime3: List[PlayerState], newDiscards: List[Card]) = {
        // It's a brand new day!
        val eog = GameState(0, 0, playersPrime3, discardPile = newDiscards)
        println(eog)
        eog.players.foreach {
            p => println(s"${p.name} achieved ${p.describeTotalVP(eog)} VP!")
        }
        eog
    }

    override def toString = {
        val pstr = players.zipWithIndex map { case (p, i) => s"Player ${i+1}:" + p.toString + "\n" } mkString

s"""\n-- Seven State --
Age: ${if(age == 0) "End" else age}, Card left: $cardsLeft
Players {
$pstr}"""
    }

}

object GameState {
    def newWonders(players: Int): List[Wonder] = Wonder.newGameWonders.take(players)

    def newGame(wonders: List[WonderSide]): GameState = {
        val hands = Card.newAgeHands(wonders.length, 1)
        GameState(players = (wonders zip hands).zipWithIndex map { case ((wonder, hand), i) => PlayerState(hand, wonder, i, wonder.res, gold = 3) })
    }
}

