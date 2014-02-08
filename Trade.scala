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
        t.costsPossible(p, toLeft, toRight)
    }

}
