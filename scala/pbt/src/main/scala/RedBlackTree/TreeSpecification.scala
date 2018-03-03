package RedBlackTree

import RedBlackTree.NonCohesive._
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Properties}

import scala.annotation.tailrec

object TreeSpecification extends Properties("Tree") {

  implicit val keyOrdering: Ordering[Key] = Key.keyOrdering
  implicit val doubleArbitrary: Arbitrary[Double] = Key.doubleArbitrary
  implicit val keyArbitrary: Arbitrary[Key] = Key.keyArbitrary

  property("§1 - Tree is immutable") =
    forAll { (vs: List[(Key, Int)]) =>
      val dvs = distinctByKey(vs)
      dvs match {
        case (k,v)::tail =>
          val t = Tree.fromList(tail)
          val nt = t.set(k, v)

          t.lookup(k).isEmpty && nt.lookup(k).contains(v)
        case _ => true
      }
    }

  property("§2 - Lookup after set shall succeed") =
    forAll { (k: Key, v: Int, vs: List[(Key, Int)]) =>
      val t = Tree.fromList(vs)
      val nt = t.set(k, v)
      nt.lookup(k).contains(v)
    }

  property("§2.1 - Lookup for all elements shall succeed") =
    forAll { (vs: List[(Key, Int)]) =>
      val dvs = distinctByKey(vs)

      val t = Tree.fromList(dvs)

      @tailrec def loop(kvs: List[(Key, Int)]): Boolean = {
        kvs match {
          case (k, v)::tail => t.lookup(k).contains(v) && loop(tail)
          case _ => true
        }
      }

      loop(dvs)
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
