package freezing.common.scala

import freezing.common.scala.errors._
import org.scalatest.FlatSpec

class ErrorsTest extends FlatSpec {
  "Errors library" should "throw IllegalArgumentException if Errors is created from an empty list" in {
    assertThrows[IllegalArgumentException] {
      Errors(List.empty[Error])
    }
  }
  it should "create Errors case class from a non-empty list" in {
    val nel = List(Error.fromThrowable(new Exception("This can be any error")))
    assert(nel.size === 1)
  }
}
