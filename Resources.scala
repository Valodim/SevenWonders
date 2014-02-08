// yes those include both resources and goods
case class Resources(
    wood: Int = 0, stone: Int = 0, clay: Int = 0, ore: Int = 0,
    glass: Int = 0, papyrus: Int = 0, cloth: Int = 0,
    dynamic: List[Resources] = List()
) {

    def isEmpty = this.forall( _ == 0 ) && dynamic.isEmpty
    // returns the number of non-null resources
    def count = this.toList count( _ > 0 )

    def &(that: Resources): Resources = Resources.fromList(this zip that map { case (l, r) => l min r }, Nil)
    def +(that: Resources): Resources = Resources.fromList(this zip that map { case (l, r) => l + r }, this.dynamic ++ that.dynamic)

    // subtract resources. this is meant to be used in a "see what's left"
    // manner, so the first argument must not have any dynamic resources!
    def -(that: Resources): Resources = {
        var left = Resources.fromList(this zip that map { case (l, r) => (l - r) max 0 })
        // if all expenses are paid, or we have no dynamic resources, never mind
        if( left.isEmpty || that.dynamic.isEmpty ) {
            return left
        }

        // otherwise, there are dynamic resources to be spent!
        // as long as the number reduces:
        var len = 0
        var dyn = that.dynamic
        do {
            len = left.count

            // step 1: eliminate all uninteresting resource values for us
            dyn = dyn map { d => (left & d) }
            // step 2a: count number of relevant resources, and elminiate all that have none
            val dyn2 = dyn zip (dyn map (_.count) ) filter ( _._2 > 0 )
            // step 3: for all with only a single relevant resource, just apply it
            dyn = dyn2 collect {
                // so non-functional~
                case (d, 1) => left = left - d; null
                case (d, c) => d
            } filter (_ != null)
        } while(left.count < len)

        // step 4: return all that are left
        Resources.fromList(left, dyn)
    }

    /** Split into singleton instances of Resources.
     * Note that dynamic resources are not supported here!
     */
    def split(): List[Resources] = {
        this.zipWithIndex flatMap { case (0, i) => Nil; case (x, i) => List.fill(x)(Resources.singleton(i)) }
    }

    override def toString() = s"[$wood / $stone / $clay / $ore // $glass / $papyrus / $cloth] " + (dynamic.mkString(", "))

}

object Resources {
    def singleton(i: Int) = i match {
        case 0 => Resources(wood = 1)
        case 1 => Resources(stone = 1)
        case 2 => Resources(clay = 1)
        case 3 => Resources(ore = 1)
        case 4 => Resources(glass = 1)
        case 5 => Resources(papyrus = 1)
        case 6 => Resources(cloth = 1)
    }

    def fromList(l: List[Int], dyn: List[Resources] = Nil): Resources = {
        val i = l.toIterator
        // not too happy with this ,but oh well
        Resources(i.next, i.next, i.next, i.next, i.next, i.next, i.next, dyn)
    }
    implicit def toList(r: Resources): List[Int] = List(r.wood, r.stone, r.clay, r.ore, r.glass, r.papyrus, r.cloth)

    // ...nicer way to do this?
    def dynamic(wood: Int = 0, stone: Int = 0, clay: Int = 0, ore: Int = 0, glass: Int = 0, papyrus: Int = 0, cloth: Int = 0) =
        Resources(dynamic=List(Resources(wood, stone, clay, ore, glass, papyrus, cloth)))
}

