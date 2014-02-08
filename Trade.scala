// must only be instantiated in here! (
case class Trade(cost: Int, toLeft: Int, toRight: Int)

object Trade {

    def offerCard(p: PlayerState, t: CardTrade): Option[ActionPickWithTrade] = {
        Some(ActionPickWithTrade(t, Trade(0, 0, 0)))
    }
    def offerWonder(p: PlayerState, t: WonderTrade): Option[ActionWonderWithTrade] = {
        None
    }

    // checks if an offer to the left and right checks out
    def checkOffer(p: PlayerState, t: TradeOption, toLeft: Int, toRight: Int): Boolean = {

        val costsLeft = List.fill(p.tradeLeft._1)(4) ::: List.fill(p.tradeLeft._2)(3)
        val costsRight = List.fill(p.tradeRight._1)(4) ::: List.fill(p.tradeRight._2)(3)

        // accumulate cost for left-only and right-only resources
        val restLeft = toLeft - (t.left zip costsLeft).map{ case (x,y) => x*y }.sum
        val restRight = toRight - (t.right zip costsRight).map{ case (x,y) => x*y }.sum

        // nothing left? early true, then
        if(restLeft == 0 && restRight == 0 && t.either.isEmpty)
            return true

        // check if what's left of our funds works for the "either" resources
        val splitres = t.either.split
        val leftright =
                splitres.map(x => (x zip costsLeft).collectFirst{ case(1,y) => y }) zip
                splitres.map(x => (x zip costsRight).collectFirst{ case(1,y) => y })

        // get a list of all possible left/right combinations
        val combinations = List.fill(splitres.length)(List(0,1)).flatten combinations(splitres.length)

        // check if any possible combination sums up to (toLeft,toRight)
        combinations map{ combination =>
            val (l: List[Int], r: List[Int]) = combination.zip(leftright).map {
                case (0,(x,_)) => (x,0)
                case (1,(_,x)) => (0,x)
            }.unzip
            ( l.sum, r.sum )
        } exists( _ == (restLeft, restRight) )

    }

}
