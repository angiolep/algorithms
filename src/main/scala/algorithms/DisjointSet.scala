package algorithms


trait DisjointSet {
  private[algorithms] def rank: Int
  def union(p: Int, q: Int): DisjointSet
  def find(x: Int): Int
}

private[algorithms] object DisjointSet {
  def make(rank: Int) = Array.tabulate[Int](rank){x => x}
}


// -----------------------------------------
// array elements are set representatives
class EagerDisjointSet private[algorithms](ids: Array[Int]) extends DisjointSet {

  private[algorithms] val rank = ids.distinct.size

  // BAD: at worse 2+2*N accesses
  def union(p: Int, q: Int): DisjointSet = {
    val idp = ids(p)
    val idq = ids(q)
    new EagerDisjointSet(
      ids.map { id =>
        if (id == idp) idq
        else id
      }
    )
  }

  // GOOD: it does one access only :-)
  def find(x: Int): Int = ids(x)
}

object EagerDisjointSet {
  def apply(rank: Int) = new EagerDisjointSet(DisjointSet.make(rank))
}



// --------------------------
// hold forest of trees; array elements are parents; roots are set representatives
class LazyDisjointSet private[algorithms](ids: Array[Int]) extends DisjointSet {

  private[algorithms] val rank = ids.map(root(_)).distinct.size

  // BAD: at worse N accesses (severely impact both union and find)
  protected def root(p: Int): Int = {
    val idp = ids(p)
    if (idp == p) p
    else root(idp)
  }

  // BAD: at worse 2*N+1 accesses
  def union(p: Int, q: Int): DisjointSet = {
    val rp = root(p)
    val rq = root(q)
    if (rp != rq) {
      new LazyDisjointSet(
        ids.updated(rp, rq)
      )
    }
    else
      this
  }

  // BAD: at worse N accesses (because of root lookup)
  def find(x: Int): Int = root(x)
}


object LazyDisjointSet {
  def apply(rank: Int) = new LazyDisjointSet(DisjointSet.make(rank))
}



// -------------------------
// improve lazy approach by keeping tree sizes minimal
class WeightedDisjointSet private[algorithms](ids: Array[Int], sizes: Array[Int])
  extends LazyDisjointSet(ids) {

  override def union(p: Int, q: Int): DisjointSet = {
    val rp = root(p)
    val rq = root(q)
    if (rp != rq) {
      // put the smaller tree below the larger tree
      if (sizes(rp) <= sizes(rq)) {
        new WeightedDisjointSet(
          ids.updated(rp, rq),
          sizes.updated(rq, sizes(rq) + sizes(rp))
        )
      } else {
        new WeightedDisjointSet(
          ids.updated(rq, rp),
          sizes.updated(rp, sizes(rq) + sizes(rp))
        )
      }
    }
    else
      this
  }
}


object WeightedDisjointSet {
  def apply(rank: Int) = new WeightedDisjointSet(DisjointSet.make(rank), Array.fill(rank)(1))
}
