package RedBlackTree

import RedBlackTree.NonCohesive.distinctByKey
import RedBlackTree.TreeSpecification.property
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties, Test}

trait Key {
  def tag: Int
}

object Key {

  case class IntKey(key: Int) extends Key {
    override def tag: Int = 1
  }

  case class DoubleKey(key: Double) extends Key {
    override def tag: Int = 2
  }

  case class StringKey(key: String) extends Key {
    override def tag: Int = 3
  }

  def genFor[T](implicit arb: Arbitrary[T]): Gen[T] = arb.arbitrary

  implicit val keyOrdering: Ordering[Key] = new Ordering[Key] {
    override def compare(x: Key, y: Key): Int = {
      (x, y) match {
        case (Key.IntKey(lk), Key.IntKey(rk)) => lk.compareTo(rk)
        case (Key.DoubleKey(lk), Key.DoubleKey(rk)) => lk.compareTo(rk)
        case (Key.StringKey(lk), Key.StringKey(rk)) => lk.compareTo(rk)
        case _ => x.tag.compareTo(y.tag)
      }
    }
  }

  implicit val keyArbitrary: Arbitrary[Key] = {
    val intKeys: Gen[Key] = genFor[Int].map(Key.IntKey)
    val specificDoubles = Gen.frequency[Double](
      (100, 0.0),
      (100, 1.0),
      (100, -1.0),
      (100, -0.0),
      (100, Double.MinValue),
      (100, Double.MinPositiveValue),
      (100, Double.PositiveInfinity),
      (100, Double.NegativeInfinity)
      //(100, Double.NaN) // TODO: Investigate why NaN don't fail more tests,
    )
    val doubles = genFor[Double]
    val doubleKeys: Gen[Key] = Gen.frequency((100, specificDoubles), (100, doubles)).map(Key.DoubleKey)
    val stringKeys: Gen[Key] = genFor[String].map(Key.StringKey)
    val keys = Gen.oneOf(intKeys, doubleKeys, stringKeys)
    Arbitrary(keys)
  }

}

object NonCohesive {
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
}

object NonCohesiveSpecification extends Properties("NonCohesive") {
  property("distinctByKey should preserve first distinct key value pair") =
    forAll { (vs: List[(Int, Int)]) =>
      val evs = vs
        .groupBy(_._1).toList
        .map { case (k, v) => (k, v.head._2) }
        .sortBy(_._1)

      val avs = distinctByKey(vs)
        .sortBy(_._1)

      evs == avs
    }
}

object Main {
  def main(argv: Array[String]) = {
    val params = Test.Parameters.default.withMinSuccessfulTests(1000)
    NonCohesiveSpecification.check(params)
    TreeSpecification.check(params)
  }
}

