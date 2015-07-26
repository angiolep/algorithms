package algorithms

// WARN: this is NOT thread-safe!

// create N-by-N grid, with all sites blocked
class PercolationSystem(val n: Int) {
  require(n > 0)

  val Blocked = false
  val Open = true

  private val top = 0
  private val siteCount = n * n + 2
  private val bottom = siteCount - 1

  // e.g. if n is 4 the siteCount is 18 (16 + 2 virtual sites)
  val disjointSet = DisjointSet(siteCount)

  // a mutable array of site states
  private val stateOf = Array.tabulate(siteCount) { site =>
    if (site == top || site == bottom) Open
    else Blocked
  }


  // is th site at (row, col) open?
  def isOpen(row: Int, col: Int): Boolean = isOpen(siteAt(row, col))


  // open the site at (row, col) and union it with its open neighbors
  def open(row: Int, col: Int): Unit = {
    val site = siteAt(row, col)
    if (!isOpen(site)) {
      open(site)
      openNeighborsOf(site).foreach {
        neighbor => disjointSet.union(site, neighbor)
      }
    }
  }

  // is the site at (row, col) fully open?
  // -> yes if it is connected with any of the top sites
  def isFull(row: Int, col: Int): Boolean = isFull(siteAt(row, col))


  // does this system percolates?
  // -> yes if the bottom site is connected with the top site
  def percolates(): Boolean = disjointSet.areConnected(bottom, top)


  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  // e.g. siteAt(3, 5) is 12
  private[algorithms] def siteAt(row: Int, col: Int) = {
    require((col >= 1 && col <= n) && (row >= 1 && row <= n), "out of bounds")
    ((row - 1) * n) + col
  }

  private def isOpen(site: Int) = stateOf(site) == Open

  private def open(site: Int) = stateOf(site) = Open


  private def isTopBorder(site: Int) = (site > top) && (site <= n)

  private def isRightBorder(site: Int) = (site != top) && (site % n == 0)

  private def isBottomBorder(site: Int) = (site > bottom - n) && (site < bottom)

  private def isLeftBorder(site: Int) = (site != top) && (site != bottom) && ((site - 1) % n) == 0


  private[algorithms] def neighborsOf(site: Int): Set[Int] = {
    val ns = collection.mutable.Set.empty[Int]
    if (!isTopBorder(site)) ns += (site - n)
    if (!isRightBorder(site)) ns += (site + 1)
    if (!isBottomBorder(site)) ns += (site + n)
    if (!isLeftBorder(site)) ns += (site - 1)
    if (isTopBorder(site)) ns += top
    if (isBottomBorder(site)) ns += bottom
    // return an immutable set
    Set() ++ ns
  }

  private def openNeighborsOf(site: Int) = neighborsOf(site).filter(isOpen)

  private def isFull(site: Int) = disjointSet.areConnected(site, top)
}