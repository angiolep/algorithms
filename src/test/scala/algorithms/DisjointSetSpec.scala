package algorithms

import org.scalatest._


// see http://www.scalatest.org/user_guide/sharing_tests

class DisjointSetSpec extends FlatSpec with DisjointSetBehaviour with MustMatchers {

  "An EagerDisjointSet" should behave like anyDisjointSet(EagerDisjointSet(10))
  it should "union given pairs of elements" in {
    EagerDisjointSet(10).union((2,0)::(1,5)::(4,2)::(1,7)::(9,0)::(0,5)::Nil)
      .toString mustBe "7 7 7 3 7 7 6 7 8 7"
  }
  it should  "find a set representative of a given element" in {
    EagerDisjointSet(10).union(5, 8).union(2, 5)
      .find(2) mustBe 8
  }

  "An LazyDisjointSet" should behave like anyDisjointSet(LazyDisjointSet(10))
  it should  "find a set representative of a given element" in {
    LazyDisjointSet(10).union(5, 8).union(2, 5)
      .find(2) mustBe 8
  }

  "An WeightedDisjointSet" should behave like anyDisjointSet(WeightedDisjointSet(10))
  it should "union given pairs of elements" in {
    WeightedDisjointSet(10).union((6,9)::(8,2)::(5,9)::(0,6)::(9,1)::(5,4)::(3,7)::(8,7)::(4,3)::Nil)
      .toString mustBe "6 6 8 8 6 6 6 3 6 6"
  }
}


trait DisjointSetBehaviour { this: FlatSpec =>

  def anyDisjointSet(dsj: DisjointSet) = {
    it should "be constructed by a given size" in {
      assert(dsj.rank == 10)
    }
    it should "union 2 given elements joining their sets" in {
      val result = dsj.union(2, 5).union(5, 8).union(2, 8).union(5, 7)
      assert(result.rank ==  7)
    }
  }
}

