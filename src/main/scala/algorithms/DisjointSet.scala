package algorithms


trait DisjointSet {
  def rank: Int
  def union(p: Int, q: Int): DisjointSet
  def find(x: Int): Int
}


// -----------------------------------------
// sequence elements are set representatives
class EagerDisjointSet(ids: IndexedSeq[Int]) extends DisjointSet {

  val rank = ids.distinct.size

  // BAD: at worse 2+2*N accesses
  def union(p: Int, q: Int): DisjointSet = {
    val idp = ids(p)
    val idq = ids(q)
    new EagerDisjointSet(ids.map { id =>
      if (id == idp) idq 
      else id
    })
  }

  // GOOD: it does one access only :-)
  def find(x: Int): Int = ids(x)
}

object EagerDisjointSet {
  def apply(rank: Int) = new EagerDisjointSet(0 until rank)
}



// --------------------------
// sequence elements are parents, roots are set representatives
class LazyDisjointSet(ids: IndexedSeq[Int]) extends DisjointSet {

  val rank = ids.map(root(_)).distinct.size

  // BAD: at worse N accesses (impact both union and find)
  private def root(p: Int): Int = {
    val idp = ids(p)
    if (idp == p) p
    else root(idp)
  }

  // BAD: at worse 2*N+1 accesses
  def union(p: Int, q: Int): DisjointSet = {
    val rp = root(p)
    val rq = root(q)
    if (rp != rq) new LazyDisjointSet(ids.updated(rp, rq))
    else this
  }

  // BAD: at worse N accesses (because of traversal up to root)
  def find(x: Int): Int = root(x)
}


object LazyDisjointSet {
  def apply(rank: Int) = new LazyDisjointSet(0 until rank)
}


