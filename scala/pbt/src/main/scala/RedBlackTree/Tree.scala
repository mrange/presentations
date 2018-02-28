package RedBlackTree

import org.scalacheck.Prop.forAll
import org.scalacheck.Properties


object Color extends Enumeration {
  val Black, Red = Value
}


trait Tree[K, V] {
  val ord: Ordering[K]

  def depth: Int

  def lookup(key: K): Option[V]

  def blackify: Tree[K, V]

  def setImpl(key: K, value: V): Tree[K, V]

  def foldLeft[S](z: S)(f: (S, K, V) => S): S

  def toList: List[(K, V)] = {
    this
      .foldLeft(List.empty[(K, V)]) { (l, k, v) => (k, v) :: l }
      .reverse
  }

  def set(key: K, value: V): Tree[K, V] = {
    this.set(key, value).blackify
  }
}

case class Leaf[K, V](implicit val ord: Ordering[K]) extends Tree[K, V] {
  override def depth: Int = 0

  override def lookup(key: K): Option[V] = None

  override def blackify: Tree[K, V] = this

  override def setImpl(key: K, value: V): Tree[K, V] = Node(Color.Red, Tree.leaf, key, value, Tree.leaf, ord)

  override def foldLeft[S](z: S)(f: (S, K, V) => S): S = z
}

case class Node[K, V](color: Color.Value, left: Tree[K, V], key: K, value: V, right: Tree[K, V], implicit val ord: Ordering[K]) extends Tree[K, V] {

  def balance: Tree[K, V] = {
    this match {
      case Node(Color.Black, Node(Color.Red, Node(Color.Red, a, xk, xv, b, _), yk, yv, c, _), zk, zv, d, _) => Node(Color.Red, Node(Color.Black, a, xk, xv, b, this.ord), yk, yv, Node(Color.Black, c, zk, zv, d, this.ord), this.ord)
      case Node(Color.Black, Node(Color.Red, a, xk, xv, Node(Color.Red, b, yk, yv, c,_ ), _), zk, zv, d, _) => Node(Color.Red, Node(Color.Black, a, xk, xv, b, this.ord), yk, yv, Node(Color.Black, c, zk, zv, d, this.ord), this.ord)
      case Node(Color.Black, a, xk, xv, Node(Color.Red, Node(Color.Red, b, yk, yv, c, _), zk, zv, d, _), _) => Node(Color.Red, Node(Color.Black, a, xk, xv, b, this.ord), yk, yv, Node(Color.Black, c, zk, zv, d, this.ord), this.ord)
      case Node(Color.Black, a, xk, xv, Node(Color.Red, b, yk, yv, Node(Color.Red, c, zk, zv, d, _), _), _) => Node(Color.Red, Node(Color.Black, a, xk, xv, b, this.ord), yk, yv, Node(Color.Black, c, zk, zv, d, this.ord), this.ord)
      case _ => this
    }
  }


  override def depth: Int = Math.max(left.depth, right.depth) + 1

  override def lookup(key: K): Option[V] = {
    val c = ord.compare(key, this.key)
    if (c < 0) {
      left.lookup(key)
    } else if (c > 0) {
      right.lookup(key)
    } else {
      Some(this.value)
    }
  }

  override def blackify: Tree[K, V] =
    if (this.color == Color.Red) {
      this.copy(color = Color.Black)
    } else {
      this
    }

  override def setImpl(key: K, value: V): Tree[K, V] = {
    val c = ord.compare(key, this.key)
    if (c < 0) {
      this.copy(left = this.left.setImpl(key, value)).balance
    } else if (c > 0) {
      this.copy(right = this.right.setImpl(key, value)).balance
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

object TreeSpecification extends Properties("Tree") {
  property("startsWith") = forAll { (a: String, b: String) =>
    (a+b).startsWith(a)
  }
}
