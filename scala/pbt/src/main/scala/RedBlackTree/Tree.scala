package RedBlackTree

import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties, Test}

object Color extends Enumeration {
  val Black, Red = Value
}

trait Tree[K, V] {
  val ord: Ordering[K]

  protected def colorBlack: Tree[K, V]

  protected def doSet(key: K, value: V): Tree[K, V]

  def depth: Int

  def lookup(key: K): Option[V]

  def foldLeft[S](z: S)(f: (S, K, V) => S): S

  def toList: List[(K, V)] = {
    this
      .foldLeft(List.empty[(K, V)]) { (l, k, v) => (k, v) :: l }
      .reverse
  }

  def set(key: K, value: V): Tree[K, V] = {
    this.doSet(key, value).colorBlack
  }
}

case class Leaf[K, V](implicit val ord: Ordering[K]) extends Tree[K, V] {
  override def depth: Int = 0

  override def lookup(key: K): Option[V] = None

  override def colorBlack: Tree[K, V] = this

  override def doSet(key: K, value: V): Tree[K, V] = Node(Color.Red, Tree.leaf, key, value, Tree.leaf, ord)

  override def foldLeft[S](z: S)(f: (S, K, V) => S): S = z
}

case class Node[K, V](color: Color.Value, left: Tree[K, V], key: K, value: V, right: Tree[K, V], implicit val ord: Ordering[K]) extends Tree[K, V] {

  def balance: Tree[K, V] = {
    this match {
      case Node(Color.Black, Node(Color.Red, Node(Color.Red, a, xk, xv, b, _), yk, yv, c, _), zk, zv, d, _) => Node(Color.Red, Node(Color.Black, a, xk, xv, b, this.ord), yk, yv, Node(Color.Black, c, zk, zv, d, this.ord), this.ord)
      case Node(Color.Black, Node(Color.Red, a, xk, xv, Node(Color.Red, b, yk, yv, c, _), _), zk, zv, d, _) => Node(Color.Red, Node(Color.Black, a, xk, xv, b, this.ord), yk, yv, Node(Color.Black, c, zk, zv, d, this.ord), this.ord)
      case Node(Color.Black, a, xk, xv, Node(Color.Red, Node(Color.Red, b, yk, yv, c, _), zk, zv, d, _), _) => Node(Color.Red, Node(Color.Black, a, xk, xv, b, this.ord), yk, yv, Node(Color.Black, c, zk, zv, d, this.ord), this.ord)
      case Node(Color.Black, a, xk, xv, Node(Color.Red, b, yk, yv, Node(Color.Red, c, zk, zv, d, _), _), _) => Node(Color.Red, Node(Color.Black, a, xk, xv, b, this.ord), yk, yv, Node(Color.Black, c, zk, zv, d, this.ord), this.ord)
      case _ => this
    }
  }


  override def depth: Int = Math.max(left.depth, right.depth) + 1

  override def lookup(key: K): Option[V] = {
    if (ord.lt(key, this.key)) {
      left.lookup(key)
    } else if (ord.lt(this.key, key)) {
      right.lookup(key)
    } else {
      Some(this.value)
    }
  }

  override def colorBlack: Tree[K, V] =
    if (this.color == Color.Red) {
      this.copy(color = Color.Black)
    } else {
      this
    }

  override def doSet(key: K, value: V): Tree[K, V] = {
    if (ord.lt(key, this.key)) {
      this.copy(left = this.left.doSet(key, value)).balance
    } else if (ord.lt(this.key, key)) {
      this.copy(right = this.right.doSet(key, value)).balance
    } else {
      this.copy(key = key, value = value)
    }
  }

  override def foldLeft[S](z: S)(f: (S, K, V) => S): S = {
    val ls = this.left.foldLeft(z)(f)
    val ss = f(ls, this.key, this.value)
    this.right.foldLeft(ss)(f)
  }
}

object Tree {
  def leaf[K, V](implicit ord: Ordering[K]): Leaf[K, V] = Leaf()

  def fromList[K, V](vs: List[(K, V)])(implicit ord: Ordering[K]): Tree[K, V] = {
    vs.foldLeft[Tree[K, V]](leaf)((s, v) => s.set(v._1, v._2))
  }
}

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

}

object TreeSpecification extends Properties("Tree") {

  override def overrideParameters(p: Test.Parameters): Test.Parameters =
    p
      .withMinSuccessfulTests(1000)

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

  property("distinctByKey should preserve first distinct key value pair") =
    forAll { (vs: List[(Int, Int)]) =>
      val evs = vs
        .groupBy(_._1).toList
        .map { case (k, v) => (k, v.head._2)}
        .sortBy(_._1)

      val avs = distinctByKey(vs)
        .sortBy(_._1)

      evs == avs
    }

  property("§1 - Tree is immutable") =
    forAll { (vs: List[(Key, Int)]) =>
      val dvs = distinctByKey(vs)
      if (dvs.nonEmpty) {
        val (k, v) = dvs.head
        val tail = dvs.tail

        val t = Tree.fromList(tail)
        val nt = t.set(k, v)

        t.lookup(k).isEmpty && nt.lookup(k).contains(v)
      } else {
        true
      }
    }

  property("§2 - Lookup after set shall succeed") =
    forAll { (k: Key, v: Int, vs: List[(Key, Int)]) =>
      val t = Tree.fromList(vs)
      val nt = t.set(k, v)
      nt.lookup(k).contains(v)
    }

  property("§4 - Parent key is greater than left child, smaller than right child") =
    forAll { (vs: List[(Key, Int)]) =>
      val t = Tree.fromList(vs)

      def check[K, V](t: Tree[K, V]): Boolean = {
        t match {
          case Node(_, l, k, _, r, ord) =>
            val lc = l match {
              case Node(_, _, lk, _, _, _) =>
                ord.lt(lk, k)
              case _ => true
            }

            val rc = r match {
              case Node(_, _, rk, _, _, _) =>
                ord.lt(k, rk)
              case _ => true
            }

            lc && rc && check(l) && check(r)
          case _ => true
        }
      }

      check(t)
    }

  property("§5 - No Red node has a Red child") =
    forAll { (vs: List[(Key, Int)]) =>
      val t = Tree.fromList(vs)

      def check[K, V](t: Tree[K, V]): Boolean = {
        t match {
          case Node(Color.Red, l, _, _, r, _) =>
            val lc = l match {
              case Node(Color.Red, _, _, _, _, _) => false
              case _ => true
            }

            val rc = r match {
              case Node(Color.Red, _, _, _, _, _) => false
              case _ => true
            }

            lc && rc && check(l) && check(r)
          case Node(_, l, _, _, r, _) =>
            check(l) && check(r)
          case _ => true
        }
      }

      check(t)
    }

  property("§6 - Every path from the Root to a Leaf contains the same number of Black nodes") =
    forAll { (vs: List[(Key, Int)]) =>
      val t = Tree.fromList(vs)

      def check[K, V](t: Tree[K, V]): Option[Int] = {
        t match {
          case Node(c, l, _, _, r, _) =>
            (check(l), check(r)) match {
              case (Some(lbc), Some(rbc)) =>
                if (lbc == rbc) {
                  Some(lbc + (if (c == Color.Black) 1 else 0))
                } else {
                  None
                }
              case _ => None
            }
          case _ => Some(1)
        }
      }

      check(t).isDefined
    }

  property("§7 - Tree depth is at most 2[log2 (n + 1)]") =
    forAll { (vs: List[(Key, Int)]) =>
      def log2(v: Double) = Math.log(v) / Math.log(2.0)

      val dvs = distinctByKey(vs)
      val t = Tree.fromList(dvs)
      val d = t.depth

      d.toDouble <= 2.0 * log2((dvs.length + 1).toDouble)
    }


}
