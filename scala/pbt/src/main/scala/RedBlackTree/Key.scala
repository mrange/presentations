package RedBlackTree

import RedBlackTree.NonCohesive._
import org.scalacheck.{Arbitrary, Gen}

sealed trait Key {
  def tag: Int
}

object Key {
  val keyOrdering: Ordering[Key] = new Ordering[Key] {
    override def compare(x: Key, y: Key): Int = {
      (x, y) match {
        case (Key.IntKey(lk), Key.IntKey(rk)) => lk.compareTo(rk)
        case (Key.DoubleKey(lk), Key.DoubleKey(rk)) => lk.compareTo(rk)
        case (Key.StringKey(lk), Key.StringKey(rk)) => lk.compareTo(rk)
        case _ => x.tag.compareTo(y.tag)
      }
    }
  }

  case class IntKey(key: Int) extends Key {
    override def tag: Int = 1
  }

  case class StringKey(key: String) extends Key {
    override def tag: Int = 3
  }

  case class DoubleKey(key: Double) extends Key {
    override def tag: Int = 2
  }

  val keyArbitrary: Arbitrary[Key] = {
    val intKeys: Gen[Key] = genFor[Int].map(Key.IntKey)
    val doubleKeys: Gen[Key] = doubleArbitrary.arbitrary.map(Key.DoubleKey)
    val stringKeys: Gen[Key] = genFor[String].map(Key.StringKey)
    val gen = Gen.oneOf(intKeys, doubleKeys, stringKeys)
    Arbitrary(gen)
  }
}
