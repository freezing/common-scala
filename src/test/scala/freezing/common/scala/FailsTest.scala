package freezing.common.scala

import freezing.common.scala.fails._
import freezing.common.scala.results.{Bad, EResult}
import org.scalatest.FlatSpec

class FailsTest extends FlatSpec {
  "Fails library" should "throw IllegalArgumentException if Fails is created from an empty list" in {
    assertThrows[IllegalArgumentException] {
      Fails(List.empty[Fail])
    }
  }
  it should "create Fails case class from a non-empty list" in {
    val nel = List(Fail.fromThrowable(new Exception("This can be any error")))
    Fails(nel)
    assert(nel.size === 1)
  }
  it should "have two Fails for the list of two Fails and one Success" in {
    val nel = List(
      EResult(1),
      EResult(Fail.fromThrowable(new Exception("This can be any error"))),
      EResult(3),
      EResult(Fail.fromThrowable(new Exception("Another error")))
    )

    assert(nel.validate.isBad)
    nel.validate match {
      case Bad(fails) => assert(fails.asInstanceOf[Fails].nel.size === 2)
      case _ => assert(false)
    }
  }
}
