package algorithms

import org.scalatest._


// see http://www.scalatest.org/user_guide/sharing_tests

class DisjointSetSpec extends FlatSpec with DisjointSetBehaviour with MustMatchers {

  "An EagerDisjointSet" should behave like aDisjointSet(EagerDisjointSet(10))

  it should "union given pairs of elements" in {
    val dsj = EagerDisjointSet(10)
    dsj.union((2,0), (1,5), (4,2), (1,7), (9,0), (0,5))
    dsj.toString mustBe "7 7 7 3 7 7 6 7 8 7"
  }

  it should "find the set representative of a given element" in {
    val dsj = EagerDisjointSet(10)
    dsj.find(2) mustBe 2
    dsj.union((5,8), (2,5))
    dsj.find(2) mustBe 8
  }


  "A LazyDisjointSet" should behave like aDisjointSet(LazyDisjointSet(10))

  it should "union given pairs of elements" in {
    val dsj = LazyDisjointSet(10)
    dsj.union((2,0), (1,5), (4,2), (1,7), (9,0), (0,5))
    dsj.toString mustBe "7 5 0 3 0 7 6 7 8 0"
  }


  "A WeightedDisjointSet" should behave like aDisjointSet(WeightedDisjointSet(10))

  it should "union given pairs of elements" in {
    val dsj = WeightedDisjointSet(10)
    dsj.union((6,9), (8,2), (5,9), (0,6), (9,1), (5,4), (3,7), (8,7), (4,3))
    dsj.toString mustBe "6 6 8 8 6 6 6 3 6 6"
  }
}


trait DisjointSetBehaviour { this: FlatSpec =>

  def aDisjointSet(dsj: DisjointSet) = {
    it should "be constructed with a given size" in {
      assert(dsj.rank == 10)
    }
    it should "union given element pairs by joining their sets" in {
      dsj.union((2,5), (5,8), (2, 8), (5, 7))
      assert(dsj.rank ==  7)
    }
  }
}

