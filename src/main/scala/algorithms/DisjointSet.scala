package algorithms


trait DisjointSet {
  private[algorithms] val ids: Array[Int]

  def rank: Int

  def union(p: Int, q: Int): Unit

  def find(x: Int): Int

  def union(pairs: (Int, Int)*): Unit = pairs.foreach { case (p, q) => union(p, q)}

  def areConnected(p: Int, q: Int): Boolean = find(p) == find(q)

  override def toString() = ids.mkString(" ")
}


object DisjointSet {
  private[algorithms] def make(size: Int) = Array.tabulate[Int](size){x => x}

  def apply(size: Int) = WeightedDisjointSet(size)
}



// -----------------------------------------
// array elements are set representatives
class EagerDisjointSet private[algorithms](reps: Array[Int]) extends DisjointSet {

  private[algorithms] val ids = reps

  def rank = ids.distinct.size

  // BAD: at worse 2+2*N accesses
  def union(p: Int, q: Int): Unit = {
    val idp = ids(p)
    val idq = ids(q)
    ids.indices.foreach { k => if (ids(k) == idp) ids(k) = idq }
  }

  // GOOD: it does one access only :-)
  def find(x: Int): Int = ids(x)
}

object EagerDisjointSet {
  def apply(size: Int) = new EagerDisjointSet(DisjointSet.make(size))
}



// --------------------------
// hold forest of trees; array elements are parents; roots are set representatives
class LazyDisjointSet private[algorithms](reps: Array[Int]) extends DisjointSet {

  private[algorithms] val ids = reps

  def rank = ids.map(root(_)).distinct.size

  // BAD: at worse N accesses (severely impact both union and find)
  protected def root(p: Int): Int = {
    val idp = ids(p)
    if (idp == p) p
    else root(idp)
  }

  // BAD: at worse 2*N+1 accesses
  def union(p: Int, q: Int): Unit = {
    val rp = root(p)
    val rq = root(q)
    if (rp != rq) ids(rp) = rq
  }

  // BAD: at worse N accesses (because of root lookup)
  def find(x: Int): Int = root(x)
}


object LazyDisjointSet {
  def apply(size: Int) = new LazyDisjointSet(DisjointSet.make(size))
}



// -------------------------
// improve lazy approach by keeping tree sizes minimal
class WeightedDisjointSet private[algorithms](reps: Array[Int], sizes: Array[Int])
  extends LazyDisjointSet(reps) {

  override def union(p: Int, q: Int): Unit = {
    val rp = root(p)
    val rq = root(q)
    if (rp != rq) {
      // put the smaller tree below the larger tree
      if (sizes(rp) >= sizes(rq)) {
        ids(rq) = rp
        sizes(rp) = sizes(rq) + sizes(rp)
      } else {
        ids(rp) = rq
        sizes(rq) = sizes(rq) + sizes(rp)
      }
    }
  }
}


object WeightedDisjointSet {
  def apply(size: Int) = new WeightedDisjointSet(DisjointSet.make(size), Array.fill(size)(1))
}
