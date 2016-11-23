package freezing.common.scala

import fails._

package object results {
  // Result that can fail
  sealed abstract class EResult[+G] {
    def isBad: Boolean = this match {
      case Bad(_) => true
      case Good(_) => false
    }

    def isGood: Boolean = this match {
      case Bad(_) => false
      case Good(_) => true
    }

    /** Catamorphism. Run the first given function if Bad, otherwise, the second given function. */
    def fold[X](bad: Fail => X, good: G => X): X = this match {
      case Bad(a) => bad(a)
      case Good(b) => good(b)
    }

    /** Map on the good of this result. */
    def map[D](g: G => D): EResult[D] = this match {
      case Good(a)     => Good(g(a))
      case b @ Bad(_) => b
    }

    /** Bind through the good of this result. */
    def flatMap[D](g: G => EResult[D]): EResult[D] = this match {
      case a @ Bad(_) => a
      case Good(b) => g(b)
    }

    /** Return an empty option or option with one element on the good of this result. Useful to sweep Fails under the carpet. */
    def toOption: Option[G] = this match {
      case Bad(_) => None
      case Good(g) => Some(g)
    }
  }

  /**
    * Used to represent the failure case of a result.
    */
  final case class Bad[B <: Fail](b: B) extends EResult[Nothing]

  /**
    * Used to represent the success case of a result.
    */
  final case class Good[G](g: G) extends EResult[G]

  object EResult {
    def apply[R](r: R): EResult[R] = Good(r)
    def apply[R](f: Fail): EResult[R] = Bad(f)
    def apply[R](t: Throwable): EResult[R] = apply(Fail.fromThrowable(t))
  }

  implicit class ValidationOps[R](l: List[EResult[R]]) {
    def validate: EResult[List[R]] = {
      val (fails, results) = l.foldLeft(List.empty[Fail], List.empty[R]) {
        case ((es, rs), Bad(f)) => (f :: es, rs)
        case ((es, rs), Good(g)) => (es, g :: rs)
      }

      fails match {
        case Nil => Good(results.reverse)
        case nel => Bad(Fails(nel.reverse))
      }
    }
  }
}