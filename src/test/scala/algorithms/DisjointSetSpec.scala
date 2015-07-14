package algorithms

import org.scalatest._

class DisjointSetSpec extends WordSpec with MustMatchers {

  "An EagerDisjointSet" should {
    "be constructed by given rank" in {
      EagerDisjointSet(10)
        .rank mustBe 10
    }
    "union sets of given elements" in {
      EagerDisjointSet(10)
        .union(2, 5)
        .union(5, 8)
        .union(2, 8)
        .union(5, 7)
        .rank mustBe 7
    }
    "find set representative by given element" in {
      EagerDisjointSet(10)
        .union(5, 8)
        .union(2, 5)
        .find(2) mustBe 8
    }
  }
}