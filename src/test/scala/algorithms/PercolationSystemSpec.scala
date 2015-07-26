package algorithms

import org.scalatest._
import org.scalatest.prop._


class PercolationSystemSpec extends FlatSpec with MustMatchers with PropertyChecks {

  "A PercolationSystem" should "throw exception if built with non-positive dimension" in {
    forAll { (n: Int) =>
      whenever (n <= 0) {
        an [IllegalArgumentException] must be thrownBy { new PercolationSystem(n) }
      }
    }
  }

  it should "start with all sites blocked" in {
    val n = 4
    val p = new PercolationSystem(n)
    for (row <- 1 to n) {
      for (col <- 1 to n) {
        p.isOpen(row, col) mustBe false
      }
    }
  }


  it should "map (row, col) to site" in {
    val p = new PercolationSystem(4)
    p.siteAt(1,1) mustBe 1
    p.siteAt(1,2) mustBe 2
    p.siteAt(1,3) mustBe 3
    p.siteAt(1,4) mustBe 4
    p.siteAt(2,1) mustBe 5
    p.siteAt(2,2) mustBe 6
    p.siteAt(2,3) mustBe 7
    p.siteAt(2,4) mustBe 8
    p.siteAt(3,1) mustBe 9
    p.siteAt(3,2) mustBe 10
    p.siteAt(3,3) mustBe 11
    p.siteAt(3,4) mustBe 12
    p.siteAt(4,1) mustBe 13
    p.siteAt(4,2) mustBe 14
    p.siteAt(4,3) mustBe 15
    p.siteAt(4,4) mustBe 16
  }


  it should "find neighbors of a site" in {
    val p = new PercolationSystem(4)
    p.neighborsOf(1) must contain theSameElementsAs Seq(0, 2, 5)
    p.neighborsOf(8) must contain theSameElementsAs Seq(4, 7, 12)
    p.neighborsOf(9) must contain theSameElementsAs Seq(5, 10, 13)
    p.neighborsOf(10) must contain theSameElementsAs Seq(6, 9, 11, 14)
    p.neighborsOf(16) must contain theSameElementsAs Seq(12, 15, 17)
  }


  it should "open a site and connect it with open neighbors" in {
    val ps = new PercolationSystem(4)
    /* s1 */  ps.open(1,1); ps.isOpen(1,1) mustBe true
    /* s2 */  ps.open(1,2); ps.isOpen(1,2) mustBe true
    /* s6 */  ps.open(2,2); ps.isOpen(2,2) mustBe true
    /* s11 */ ps.open(3,3); ps.isOpen(3,3) mustBe true
    /* s15 */ ps.open(4,3); ps.isOpen(4,3) mustBe true
  }


  it should "test if it percolates" in {
    val ps = new PercolationSystem(4)
    /* s1 */  ps.open(1,1); ps.isFull(1,1) mustBe true
    /* s2 */  ps.open(1,2); ps.isFull(1,2) mustBe true
    /* s6 */  ps.open(2,2); ps.isFull(2,2) mustBe true
    /* s15 */ ps.open(4,3); ps.isFull(4,3) mustBe false
    /* s11 */ ps.open(3,3); ps.isFull(3,3) mustBe false
    /* s10 */ ps.open(3,2); ps.isFull(3,2) mustBe true
    ps.isFull(3,3) mustBe true
    ps.isFull(4,3) mustBe true

    ps.percolates() mustBe true
  }
}