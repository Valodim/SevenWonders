// must only be instantiated in here! (
case class Trade(toLeft: Int, toRight: Int) {

    lazy val cost = toLeft + toRight

    def tradeOffer(p: PlayerState, t: CardTrade): Option[ActionPickWithTrade] = {
        if(checkOffer(p, t))
            Some(ActionPickWithTrade(t, this))
        else
            None
    }
    def tradeOffer(p: PlayerState, t: WonderTrade, card: CardOption): Option[ActionWonderWithTrade] = {
        if(checkOffer(p, t))
            Some(ActionWonderWithTrade(t, this, card))
        else
            None
    }

    // checks if an offer to the left and right checks out
    def checkOffer(p: PlayerState, t: TradeOption): Boolean = {

        // accumulate cost for left-only and right-only resources
        val (costLeft, costRight) = t.fixedSums(p)
        val (restLeft, restRight) = (toLeft - costLeft, toRight - costRight)

        // nothing left? early true, then
        if(restLeft == 0 && restRight == 0 && t.either.isEmpty)
            return true

        // check if what's left of our funds works for the "either" resources
        val leftright = t.splitCosts(p)

        // get a list of all possible left/right combinations
        val combinations = List.fill(leftright.length)(List(0,1)).flatten combinations(leftright.length)

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
