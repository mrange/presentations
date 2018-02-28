import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

object TreeSpecification extends Properties("Tree") {

  property("startsWith") = forAll { (a: String, b: String) =>
    (a+b).startsWith(a)
  }
}
