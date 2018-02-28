import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

class TreeSpecification extends Properties("Tree") {
  property("startsWith") = forAll { (a: String, b: String) =>
    ("2" + a+b+"1").startsWith(a)
  }
}
