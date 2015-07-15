package algorithms

import org.scalatest._


// see http://www.scalatest.org/user_guide/sharing_tests

class DisjointSetSpec extends FlatSpec with DisjointSetBehaviour {
  "An EagerDisjointSet" should behave like anyDisjointSet(EagerDisjointSet(10))
  "An LazyDisjointSet" should behave like anyDisjointSet(LazyDisjointSet(10))
  "An WeightedDisjointSet" should behave like anyDisjointSet(WeightedDisjointSet(10))
}


trait DisjointSetBehaviour { this: FlatSpec =>

  def anyDisjointSet(dsj: DisjointSet) = {
    it should "be constructed by a given size" in {
      assert(dsj.rank == 10)
    }
    it should "join sets by union of given elements" in {
      val result = dsj.union(2, 5).union(5, 8).union(2, 8).union(5, 7)
      assert(result.rank ==  7)
    }
    it should  "find a set representative by a given element" in {
      val result = dsj.union(5, 8).union(2, 5)
      assert(result.find(2) == 8)
    }
  }
}

