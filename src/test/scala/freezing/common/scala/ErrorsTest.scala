package freezing.common.scala

import freezing.common.scala.errors._
import freezing.common.scala.results.{Bad, EResult}
import org.scalatest.FlatSpec

class ErrorsTest extends FlatSpec {
  "Errors library" should "throw IllegalArgumentException if Errors is created from an empty list" in {
    assertThrows[IllegalArgumentException] {
      Errors(List.empty[Error])
    }
  }
  it should "create Errors case class from a non-empty list" in {
    val nel = List(Error.fromThrowable(new Exception("This can be any error")))
    Errors(nel)
    assert(nel.size === 1)
  }
  it should "have two errors for the list of two Errors and one Success" in {
    val nel = List(
      EResult(1),
      EResult(Error.fromThrowable(new Exception("This can be any error"))),
      EResult(3),
      EResult(Error.fromThrowable(new Exception("Another error")))
    )

    assert(nel.validate.isBad)
    nel.validate match {
      case Bad(errors) => assert(errors.asInstanceOf[Errors].nel.size === 2)
    }
  }
}
