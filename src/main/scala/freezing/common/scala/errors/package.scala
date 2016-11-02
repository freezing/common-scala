package freezing.common.scala

package object errors {
  trait Error
  case class EThrowable(t: Throwable) extends Error
  case class Errors(nel: List[Error]) extends Error {
    // Check that nel is non-empty list
    if (nel.isEmpty) throw new IllegalArgumentException(s"Errors can't be created with an empty list!")
  }

  object Error {
    def fromThrowable(t: Throwable): EThrowable = EThrowable(t)
  }
}
