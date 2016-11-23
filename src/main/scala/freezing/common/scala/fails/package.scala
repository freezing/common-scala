package freezing.common.scala

package object fails {
  trait Fail
  case class FThrowable(t: Throwable) extends Fail
  case class Fails(nel: List[Fail]) extends Fail {
    // Check that nel is non-empty list
    if (nel.isEmpty) throw new IllegalArgumentException(s"Errors can't be created with an empty list!")
  }

  object Fail {
    def fromThrowable(t: Throwable): FThrowable = FThrowable(t)
  }
}
