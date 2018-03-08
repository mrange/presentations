package RedBlackTree

import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties, Test}

object NonCohesive {
  def genFor[T](implicit arb: Arbitrary[T]): Gen[T] = arb.arbitrary

  def distinctByKey[K, V](vs: List[(K, V)]): List[(K, V)] = {
    val s = vs.foldLeft((Set.empty[K], List.empty[(K, V)]))((s, v) => {
      if (s._1(v._1)) {
        s
      } else {
        (s._1 + v._1, v :: s._2)
      }
    })
    s._2.reverse
  }

  val doubleArbitrary: Arbitrary[Double] = {
    val specificDoubles = Gen.frequency[Double](
      (100, 0.0),
      (100, 1.0),
      (100, -1.0),
      (10, -0.0), // Negative zero is a thing in IEEE doubles
      (10, Double.MinValue),
      (10, Double.MinPositiveValue),
      (10, Double.PositiveInfinity),
      (10, Double.NegativeInfinity),
      (10, Double.NaN)
    )
    val doubles = genFor[Double]
    val gen: Gen[Double] = Gen.frequency((100, specificDoubles), (100, doubles))
    Arbitrary(gen)
  }
}

object NonCohesiveSpecification extends Properties("NonCohesive") {
  property("distinctByKey should preserve first distinct key value pair") =
    forAll { (vs: List[(Int, Int)]) =>
      val evs = vs
        .groupBy(_._1).toList
        .map { case (k, v) => (k, v.head._2) }
        .sortBy(_._1)

      val dvs = NonCohesive.distinctByKey(vs)

      val avs = dvs.sortBy(_._1)

      evs == avs
    }
}

object Main {
  def main(argv: Array[String]) = {
    val params = Test.Parameters.default.withMinSuccessfulTests(1000)

    NonCohesiveSpecification.check(params)
//    TreeSpecification.check(params)
  }
}
