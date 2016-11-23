package freezing.common.scala

import freezing.common.scala.errors._
import freezing.common.scala.results.EResult
import org.scalatest.FlatSpec

class ResultTest extends FlatSpec {
  "Result library" should "be Good if List of Good values is validated" in {
    val ints = List(EResult(5), EResult(3), EResult(7))
    val validated = ints.validate
    assert(validated === EResult(List(5, 3, 7)))
  }


}
