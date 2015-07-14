package algorithms

// sequence elements are set representatives
class EagerDisjointSet(ids: IndexedSeq[Int]) {

  val rank = ids.distinct.size

  // at worse 2+2*N accesses
  def union(p: Int, q: Int) = {
    val idp = ids(p)
    val idq = ids(q)
    new EagerDisjointSet(ids.map { id =>
      if (id == idp) idq 
      else id
    })
  }

  // GOOD: it does one access only :-)
  def find(x: Int) = ids(x)
}

object EagerDisjointSet {
  def apply(rank: Int) = new EagerDisjointSet(0 until rank)
}
